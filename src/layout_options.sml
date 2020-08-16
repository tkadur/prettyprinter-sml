structure LayoutOptions = struct
  datatype t
    = LayoutOptions of
      { doc_width: DocWidth.t
      }
end