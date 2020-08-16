structure SimpleDocStream = struct
  datatype t
    = Failure
    | Empty
    | Char of char * t
    | String of string * t
    | Line of
      { next_line_indent: int
      , doc_stream: t
      }

  local
    fun go doc_stream =
      case doc_stream of
        Failure => raise Fail "Tried to render `Failure`"
      | Empty => []
      | Char (c, doc_stream') => String.str c :: go doc_stream'
      | String (s, doc_stream') => s :: go doc_stream'
      | Line { next_line_indent, doc_stream = doc_stream'} =>
          "\n" :: Util.spaces next_line_indent :: go doc_stream'
  in
    val render = String.concat o go
  end
end