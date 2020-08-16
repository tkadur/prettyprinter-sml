structure Doc = struct
  infixr 6 <> <+>

  (**
   * Maximum number of characters that fit in one line. The layout algorithms
   * will try not to exceed the set limit by inserting line breaks when
   * applicable (e.g. via `softline`).
   *)
  structure DocWidth = struct
    datatype t
      = Bounded of
        { columns: int
        }
      | Unbounded
  end

  datatype t
    (**
     * Occurs when flattening a line. The layouter will reject this
     * document, choosing a more suitable rendering.
     *)
    = Failure

    (** The empty document *)
    | Empty

    (** Invariant: not '\n' *)
    | Char of char

    (**
     * Invariants:
     *  - At least two characters long
     *  - Does not contain '\n'.
     *
     * For empty documents, there is `Empty`.
     * For singleton documents, there is `Char`.
     * Newlines should be replaced by e.g. `Line`.
     *)
    | String of string

    (** Hard line break *)
    | Line

    (**
     * Lay out `primary`, but when flattened (via `group`), prefer
     * `alternative`.

     * The layout algorithms work under the assumption that the first
     * alternative is less wide than the flattened second alternative.
     *)
    | FlatAlt of
      { primary: t
      , alternative: t
      }

    (** Concatenation of two documents *)
    | Concat of t * t

    (** Docment indented by a number of columns *)
    | Nest of
      { indentation: int
      , doc: t
      }

    (**
     * Used to implement layout alternatives for `group`.
     *
     * Invariant: The first lines of first document should be longer than
     * the first lines of the second one, so the layout algorithm can pick
     * the one that fits best.
     *)
    | Union of
      { primary: t
      , alternative: t
      }

    (**
     * React on the current cursor column.
     * @see `column`.
     *)
    | WithColumn of int -> t

    (**
     * React on the document's width.
     * @see `column`.
     *)
    | WithDocWidth of DocWidth.t -> t

    (**
     * React on the current nesting level.
     * @see `nesting`
     *)
    | WithNesting of int -> t

  (* TODO(tkadur): Semigroup and monoid instances *)

  (* Really basic combinators that mostly just rename constructors *)

  val empty = Empty

  val hardline = Line

  val flat_alt = FlatAlt

  val op <> = Concat

  fun nest indentation doc =
    case indentation of
      0 => doc (* Optimization *)
    | _ => Nest { indentation = indentation, doc = doc }

  val with_column = WithColumn

  val with_doc_width = WithDocWidth

  (* Utility stuff *)

  fun concatWith f docs =
    case docs of
      [] => empty
    | doc :: docs' => List.foldl f doc docs'

  (* Line breaks *)

  val line = FlatAlt { primary = Line, alternative = Char #" " }

  val line' = FlatAlt { primary = Line, alternative = empty }

  val softline = Union { primary = Char #" ", alternative = Line }

  val softline' = Union { primary = empty, alternative = Line }

  (* Other whitespace *)

  fun spaces n =
    if n <= 0 then
      Empty
    else if n = 1 then
      Char #" "
    else
      String (String.implode (List.tabulate (n, fn _ => #" ")))

  fun with_width doc f =
    with_column (fn startCol => doc <> with_column (fn endCol => f (endCol - startCol)))

  fun fill n doc = with_width doc (fn width => spaces (n - width))

  fun fillBreak n doc =
    with_width doc (fn width => if width > n then nest n line' else spaces (n - width))

  (* `group` *)

  local
    structure Flatten = struct
      datatype 'a result
        (**
        * @params result
        * @param result likely flatter than the input
        *)
        = Flattened of 'a

        (** The input was already flat, e.g. a `String` *)
        | AlreadyFlat

        (** The input couldn't be flattened - it contained a `Line` or `Fail`. *)
        | NeverFlat

      fun map f result =
        case result of
          Flattened x => Flattened (f x)
        | AlreadyFlat => AlreadyFlat
        | NeverFlat => NeverFlat

      (** Flatten, but don't report whether anything changes. *)
      fun flatten doc =
        case doc of
          Failure => doc
        | Empty => doc
        | Char _ => doc
        | String _ => doc
        | Line => Failure
        | FlatAlt { alternative, ... } => flatten alternative
        | Concat (doc1, doc2) => Concat (flatten doc1, flatten doc2)
        | Nest { indentation, doc } => Nest { indentation = indentation, doc = flatten doc }
        | Union { primary, ... } => flatten primary
        | WithColumn f => WithColumn (flatten o f)
        | WithDocWidth f => WithDocWidth (flatten o f)
        | WithNesting f => WithNesting (flatten o f)

      (**
       * Choose the first element of each `Union`, and discard the first field
       * of all `FlatAlt`s.
       *
       * The result is `Flattened` if the element might change depending on the
       * layout * algorithm (i.e. contains differently renderable sub-
       * documents), and `AlreadyFlat` if the document is static (e.g. contains
       * only a plain `Empty` node). `NeverFlat` is returned when the document
       * cannot be flattened because it contains a hard `Line` or `Fail`.
       *)
      fun changes doc =
        case doc of
          Failure => NeverFlat
        | Empty => AlreadyFlat
        | Char _ => AlreadyFlat
        | String _ => AlreadyFlat
        | Line => NeverFlat
        | FlatAlt { alternative, ... } => Flattened (flatten alternative)
        | Concat (doc1, doc2) =>
          ( case (changes doc1, changes doc2) of
              (NeverFlat, _) => NeverFlat
            | (_, NeverFlat) => NeverFlat
            | (Flattened doc1', Flattened doc2') => Flattened (Concat (doc1', doc2'))
            | (Flattened doc1', AlreadyFlat) => Flattened (Concat (doc1', doc2))
            | (AlreadyFlat, Flattened doc2') => Flattened (Concat (doc1, doc2'))
            | (AlreadyFlat, AlreadyFlat) => AlreadyFlat
          )
        | Nest { indentation, doc } =>
            map (fn doc' => Nest { indentation = indentation, doc = doc' }) (changes doc)
        | Union { primary, ... } => Flattened primary
        | WithColumn f => Flattened (WithColumn (flatten o f))
        | WithDocWidth f => Flattened (WithDocWidth (flatten o f))
        | WithNesting f => Flattened (WithNesting (flatten o f))
    end
  in
    fun group doc =
      case doc of
        Union _ => doc
      | FlatAlt { primary, alternative} =>
        ( case Flatten.changes alternative of
            Flatten.Flattened alternative' =>
              Union { primary = alternative', alternative = primary }
          | Flatten.AlreadyFlat =>
              Union { primary = alternative, alternative = primary }
          | Flatten.NeverFlat => primary
        )
      | _ =>
        ( case Flatten.changes doc of
            Flatten.Flattened doc' => Union { primary = doc', alternative = doc }
          | Flatten.AlreadyFlat => doc
          | Flatten.NeverFlat => doc
        )
  end

  (* Basic concatenation *)

  fun doc1 <+> doc2 = doc1 <> Char #" " <> doc2

  val hsep = concatWith (op <+>)

  val vsep = concatWith (fn (doc1, doc2) => doc1 <> line <> doc2)

  val fill_sep = concatWith (fn (doc1, doc2) => doc1 <> softline <> doc2)

  val sep = group o vsep

  val hcat = concatWith (op <>)

  val vcat = concatWith (fn (doc1, doc2) => doc1 <> line' <> doc2)

  val fill_cat = concatWith (fn (doc1, doc2) => doc1 <> softline' <> doc2)

  val cat = group o vcat

  (* Converting from strings *)

  fun unsafe_string_without_newlines s =
    case String.explode s of
      [] => Empty
    | [c] => Char c
    | _ => String s

  val string = vsep o List.map unsafe_string_without_newlines o String.fields (fn c => c = #"\n")

  (* Indentation *)

  fun align doc =
    with_column (fn col => WithNesting (fn nesting => nest (col - nesting) doc))

  fun hang indentation doc = align (nest indentation doc)

  fun indent indentation doc = hang indentation (spaces indentation <> doc)

  (* Enclosing and separators *)

  fun enclose { left, right, doc } = left <> doc <> right

  local
    fun go punctuation docs =
      case docs of
        [] => []
      | [doc] => [doc]
      | doc :: docs' => (doc <> punctuation) :: go punctuation docs'
  in
    fun punctuate { punctuation, docs } = go punctuation docs
  end

  local
    fun intercalateSep sep docs =
      case docs of
        [] => raise Fail "Impossible"
      | [doc] => doc
      | doc :: docs' => doc <> sep <> intercalateSep sep docs'
  in
    fun encloseSep { left, right, sep, docs } =
      case docs of
        [] => left <> right
      | _ => left <> intercalateSep sep docs <> right
  end

  (* TODO(tkadur): fusion *)

end