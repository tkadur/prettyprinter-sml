signature DOC = sig
  type t

  (** The empty document. Equivalent to `string ""`. *)
  val empty: t

  (** Concatenates two documents with no space in between *)
  val <:> : t * t -> t

  (** Concatenates two documents with a space in between *)
  val <+> : t * t -> t

  (**
   * Converts a `char` to a document.
   *
   * Preconditions:
   *  - Input cannot be a newline.
   *)
  val char: char -> t

  (**
   * Converts a `string` to a document, automatically replacing all newlines
   * with `line`.
   *)
  val string: string -> t

  val unsafe_string_without_newlines: string -> t

  (**
   * Advances to the next line and indents to the current nesting level.
   * Gets replaced with `space` when `group`ed.
   *)
  val line: t

  (**
   * Advances to the next line and indents to the current nesting level.
   * Gets replaced with `empty` when `group`ed.
   *)
  val line': t

  (**
   * Behaves like `space` if the resulting output fits in a line. Otherwise,
   * behaves like `line`.
   *)
  val softline: t

  (**
   * Behaves like `empty` if the resulting output fits in a line. Otherwise,
   * behaves like `line`.
   *)
  val softline': t

  (**
   * Always inserts a newline, even when `group`ed or the output could have
   * fit on one line. Could still be discarded if it's within a `flat_alt`
   * that gets `group`ed.
   *)
  val hardline: t

  (** A single space *)
  val space: t

  (**
   *`spaces n` produces `n` spaces.
   * Somewhat more efficient than using `space` `n` times.
   *)
  val spaces: int -> t

  (**
   * `group doc` tried laying out `doc` into a single line by removing line breaks.
   * If this fails because of a `hardline` or because the output doesn't fit,
   * `doc` is laid out without any changes.
   *)
  val group: t -> t

  (**
   * `flat_alt { primary, alternative }` renders `primary` by default.
   * However, when `group`ed, `alternative` will be preferred with `primary`
   * as the fallback in case `primary` doesn't fit.
   *
   * Preconditions:
   *  - `primary` must be less wide than `alternative`.
   *)
  val flat_alt: { primary: t, alternative: t } -> t

  (**
   * `nest n doc` lays out `doc` with the nesting level (indentation of the
   * following lines) increased by `n`. Negative values of `n` are allowed
   * for decreasing the nesting value.
   *)
  val nest: int -> t -> t

  (**
   * `align doc` lays out `doc` with the nesting level set to the current
   * column.
   *)
  val align: t -> t

  (**
   * `hang n doc` lays out `doc` with the nesting level set to the current
   * column plus `n`. Negative values of `n` are allowed for decreasing the
   * nesting value.
   *)
  val hang: int -> t -> t

  (** `indent n doc` indents `doc` by `n` columns, starting from the current
   * cursor position.
   *)
  val indent: int -> t -> t

  (** Concatenate all documents element-wise with a binary function. *)
  val concat_with: (t * t -> t) -> t list -> t

  (**
   * Concatenates documents horizontally with `<+>`.
   *
   * This means that unlike `hcat`, there are spaces between documents.
   *)
  val hsep: t list -> t

  (**
   * Concatenates documents with `line` between them.
   *
   * This means that unlike `vcat`, the newlines are replaced with spaces
   * when `group`ed.
   *)
  val vsep: t list -> t

  (**
   * Concatenates docments horizontally with `<+>` as long as they fit the page,
   * then inserts a `line`. Continues this for all documents in the input.
   *)
  val fill_sep: t list -> t

  (**
   * Concatenates documents with spaces if they all fit on a single line and
   * with newlines if they don't.
   *)
  val sep: t list -> t

  (**
   * Concatenates documents horizontally with `<>`.
   *
   * This means that unlike `hsep`, there are no spaces between documents.
   *)
  val hcat: t list -> t

  (**
   * Concatenates documents with `line'` between them.
   *
   * This means that unlike `vsep`, the newlines are removed when `group`ed.
   *)
  val vcat: t list -> t

  (**
   * Concatenates docments horizontally with `<>` as long as they fit the page,
   * then inserts a `line'`. Continues this for all documents in the input.
   *)
  val fill_cat: t list -> t

  (**
   * Concatenates documents with no space between them if they all fit on a
   * single line and with newlines if they don't.
   *)
  val cat: t list -> t

  (** `enclose { left, right, doc }` is defined as `left <:> doc <:> right` *)
  val enclose: { left: t, right: t, doc: t } -> t

  (**
   * `encloseSep { left, right, sep, docs } concatenates `docs` separated by
   * `sep`, and encloses the resulting document by `left` and `right`.
   * The documents are laid out horizontally if that fits the page.
   * Otherwise, the input is split into lines entry-wise and they are laid out
   * vertically, with separators put in the front.
   *)
  val enclose_sep: { left: t, right: t, sep: t, docs: t list } -> t

  (** Appends `punctuation` to all but the last document in `docs`. *)
  val punctuate: { punctuation: t, docs: t list } -> t list

  (** Lays out a document based on what column it starts at *)
  val with_column: (int -> t) -> t

  (** Lays out a document based on the current nesting level. *)
  val with_nesting: (int -> t) -> t

  (**
   *`fill n doc` lays out `doc` then appends spaces until the width is equal
   * to `n`. If the width of `doc` is already at least `n`, nothing is appended.
   *)
  val fill: int -> t -> t

  (**
   *`fill_break n doc` lays out `doc` then appends spaces until the width is equal
   * to `n`. If the width of `doc` is already at least `n`, the nesting level is
   * increased by `n` and a `line` is appended.
   *)
  val fill_break: int -> t -> t

  (**
   * Lays out a document into a `SimpleDocStream.t`, which can then be rendered
   * into various formats.
   *)
  val layout: LayoutOptions.t -> t -> SimpleDocStream.t
end