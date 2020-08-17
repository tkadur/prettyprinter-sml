signature SIMPLE_DOC_STREAM = sig
  datatype t
    = Failure
    | Empty
    | Char of char * t
    | String of string * t
    | Line of
      { next_line_indent: int
      , doc_stream: t
      }

  (** Renders to a string *)
  val render: t -> string
end