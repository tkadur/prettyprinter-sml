structure DocWidth = struct
  (**
  * Maximum number of characters that fit in one line. The layout algorithms
  * will try not to exceed the set limit by inserting line breaks when
  * applicable (e.g. via `softline`).
  *)
  datatype t
    = Bounded of
      { line_length: int
      }
    | Unbounded
end