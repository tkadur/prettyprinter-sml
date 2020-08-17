structure Doc :> DOC = struct
  open DocWidth
  open LayoutOptions

  structure S = SimpleDocStream

  infixr 6 <:> <+>

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
     * React on the current nesting level.
     * @see `nesting`
     *)
    | WithNesting of int -> t

  (* TODO(tkadur): Semigroup and monoid instances *)

  (* Basic stuff that mostly just renames constructors *)

  val empty = Empty

  val hardline = Line

  val flat_alt = FlatAlt

  val op <:> = Concat

  fun nest indentation doc =
    case indentation of
      0 => doc (* Optimization *)
    | _ => Nest { indentation = indentation, doc = doc }

  val with_column = WithColumn

  val with_nesting = WithNesting

  (* Utility stuff *)

  (* Oh no, the value restriction! (later on) *)
  fun concat_with f (docs: t list) =
    case docs of
      [] => empty
    | _ => Internal.Util.List.foldr1 f docs

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
      String (Internal.Util.spaces n)

  val space = spaces 1

  fun with_width doc f =
    with_column (fn startCol => doc <:> with_column (fn endCol => f (endCol - startCol)))

  fun fill n doc = with_width doc (fn width => spaces (n - width))

  fun fill_break n doc =
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

  fun doc1 <+> doc2 = doc1 <:> Char #" " <:> doc2

  val hsep = concat_with (op <+>)

  val vsep = concat_with (fn (doc1, doc2) => doc1 <:> line <:> doc2)

  val fill_sep = concat_with (fn (doc1, doc2) => doc1 <:> softline <:> doc2)

  val sep = group o vsep

  val hcat = concat_with (op <:>)

  val vcat = concat_with (fn (doc1, doc2) => doc1 <:> line' <:> doc2)

  val fill_cat = concat_with (fn (doc1, doc2) => doc1 <:> softline' <:> doc2)

  val cat = group o vcat

  (* Converting from chars/strings *)

  fun char c =
    case c of
      #"\n" => raise Fail "char does not accept newlines. Consider using line instead."
    | _ => Char c

  fun unsafe_string_without_newlines s =
    case String.explode s of
      [] => Empty
    | [c] => Char c
    | _ => String s

  val string =
    vsep
      o List.map unsafe_string_without_newlines
      o String.fields (fn c => c = #"\n")

  (* Indentation *)

  fun align doc =
    with_column (fn col => with_nesting (fn nesting => nest (col - nesting) doc))

  fun hang indentation doc = align (nest indentation doc)

  fun indent indentation doc = hang indentation (spaces indentation <:> doc)

  (* Enclosing and separators *)

  fun enclose { left, right, doc } = left <:> doc <:> right

  local
    fun go punctuation docs =
      case docs of
        [] => []
      | [doc] => [doc]
      | doc :: docs' => (doc <:> punctuation) :: go punctuation docs'
  in
    fun punctuate { punctuation, docs } = go punctuation docs
  end

  local
    fun zip_with_seps sep docs =
      case docs of
        [] => []
      | doc :: docs' => (sep <:> doc) :: zip_with_seps sep docs'
  in
    fun enclose_sep { left, right, sep, docs } =
      case docs of
        [] => left <:> right
      | doc :: docs' => cat ((left <:> doc) :: zip_with_seps sep docs' @ [right])
  end

  (* TODO(tkadur): fusion *)

  (* Layout *)

  type fitting_args
    = { line_indent: int
      , curr_col: int
      , alt_initial_indent: int option
      , doc_stream: SimpleDocStream.t
      }

  type fitting_predicate = fitting_args -> bool

  type layout_pipeline = { nesting_level: int, doc: t } list

  fun layout_wadler_leijen fits doc_width doc =
    let
      fun initial_indentation doc_stream =
        case doc_stream of
          S.Line { next_line_indent, ... } => SOME next_line_indent
        | _ => NONE

      (**
       * Select the better fitting of two documents:
       * `primary` if it fits, otherwise `alternative`.

       * The fit of `alternative` is _not_ checked! It is ultimately the user's
       * responsibility to provide an alternative that can fit the page even when
       * `primary` doesn't.
       *)
      fun select_nicer { curr_nesting_level, curr_col } { primary, alternative } =
        if fits
          { line_indent = curr_nesting_level
          , curr_col = curr_col
          , alt_initial_indent = initial_indentation alternative
          , doc_stream = primary
          } then
          primary
        else
          alternative

      (**
       * Preconditions:
       *  - current column > current nesting level
       *  - current column - current indentation = number of chars in line
       *)
      fun best (curr as { curr_nesting_level, curr_col }) layout_pipeline =
        case layout_pipeline of
          [] => S.Empty
        | { nesting_level, doc } :: layout_pipeline' =>
          ( case doc of
              Failure => S.Failure
            | Empty => best curr layout_pipeline'
            | Char c =>
                S.Char
                  (c
                  , best
                      { curr_nesting_level = curr_nesting_level
                      , curr_col = curr_col + 1
                      }
                      layout_pipeline'
                  )
            | String s =>
                S.String
                  (s
                  , best
                      { curr_nesting_level = curr_nesting_level
                      , curr_col = curr_col + String.size s
                      }
                      layout_pipeline'
                  )
            | Line =>
                let
                  val doc_stream =
                    best
                      { curr_nesting_level = nesting_level, curr_col = nesting_level } layout_pipeline'

                  val nesting_level' = case doc_stream of
                    S.Empty => 0
                  | S.Line _ => 0
                  | _ => nesting_level
                in
                  S.Line { next_line_indent = nesting_level', doc_stream = doc_stream }
                end
            | FlatAlt { primary, ... } =>
                best
                  curr
                  ({ nesting_level = nesting_level, doc = primary }
                    :: layout_pipeline')
            | Concat (doc1, doc2) =>
                best
                  curr
                  ({ nesting_level = nesting_level, doc = doc1 }
                    :: { nesting_level = nesting_level, doc = doc2 }
                    :: layout_pipeline')
            | Nest { indentation, doc } =>
                best
                  curr
                  ({ nesting_level = nesting_level + indentation, doc = doc }
                    :: layout_pipeline')
            | Union { primary, alternative } =>
                let
                  val primary_stream =
                    best
                      curr
                      ({ nesting_level = nesting_level, doc = primary }
                        :: layout_pipeline')
                  val alternative_stream =
                    best
                      curr
                      ({ nesting_level = nesting_level, doc = alternative }
                        :: layout_pipeline')
                in
                  select_nicer
                    curr
                    { primary = primary_stream, alternative = alternative_stream }
                end
            | WithColumn f =>
                best
                curr
                ({ nesting_level = nesting_level, doc = f curr_col}
                  :: layout_pipeline')
            | WithNesting f =>
                best
                curr
                ({ nesting_level = nesting_level, doc = f nesting_level}
                  :: layout_pipeline')
          )
    in
      best
        { curr_nesting_level = 0, curr_col = 0 }
        [{ nesting_level = 0, doc = doc }]
    end

  local
    (* TODO(tkadur): document this *)
    fun fails_on_first_line doc_stream =
      case doc_stream of
        S.Failure => true
      | S.Empty => false
      | S.Char (_, doc_stream) => fails_on_first_line doc_stream
      | S.String (_, doc_stream) => fails_on_first_line doc_stream
      | S.Line _ => false
  in
    val layout_unbounded =
      layout_wadler_leijen
        (fn ({ doc_stream, ... }: fitting_args) =>
            not (fails_on_first_line doc_stream))
        Unbounded
  end

  fun layout (LayoutOptions { doc_width }) doc =
    case doc_width of
      Unbounded => layout_unbounded doc
    | Bounded { line_length } =>
      let
        fun fits { line_indent, curr_col, alt_initial_indent, doc_stream } =
          let
            val available_width = line_length + line_indent - curr_col

            (* TODO(tkadur): document the purpose of this *)
            val min_nesting_level =
              case alt_initial_indent of
                (* The alternate could be a (less wide) hanging layout. If so
                 * let's check the primary a bit more thoroughly so we don't
                 * miss a potentially better fitting alternate.
                 *)
                SOME indent => Int.min (indent, curr_col)
                (* The alternate definitely isn't a hanging layout. Let's check
                 * the primary with the same min_nesting_level that any subsequent
                 * lines with the same indentation use.
                 *)
              | NONE => curr_col

            fun go width doc_stream =
              if width < 0 then
                false
              else
                case doc_stream of
                  S.Failure => false
                | S.Empty => true
                | S.Char (_, doc_stream) => go (width - 1) doc_stream
                | S.String (s, doc_stream) =>
                  go (width - String.size s) doc_stream
                | S.Line { next_line_indent, doc_stream } =>
                  if min_nesting_level < next_line_indent then
                    go (line_length - next_line_indent) doc_stream
                  else
                    true
          in
            go available_width doc_stream
          end
      in
        layout_wadler_leijen fits doc_width doc
      end
end