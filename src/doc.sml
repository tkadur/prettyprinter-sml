structure Doc :> DOC = struct
  (**
   * Maximum number of characters that fit in one line. The layout algorithms
   * will try not to exceed the set limit by inserting line breaks when
   * applicable (e.g. via `softline`).
   *)
  datatype doc_width
    = Bounded of
      { columns: int
      }
    | Unbounded

  datatype doc
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
      { primary: doc
      , alternative: doc
      }

    (** Concatenation of two documents *)
    | Concat of doc * doc

    (** Docment indented by a number of columns *)
    | Nest of
      { indent: int
      , doc: doc
      }

    (**
     * Used to implement layout alternatives for `group`.
     *
     * Invariant: The first lines of first document should be longer than
     * the first lines of the second one, so the layout algorithm can pick
     * the one that fits best.
     *)
    | Union of doc * doc

    (**
     * React on the current cursor column.
     * @see `column`.
     *)
    | Column of int -> doc

    (**
     * React on the document's width.
     * @see `column`.
     *)
    | WithDocWidth of doc_width -> doc

    (**
     * React on the current nesting level.
     * @see `nesting`
     *)
    | Nesting of int -> doc
end