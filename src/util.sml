structure Util = struct
  fun spaces n = String.implode (List.tabulate (n, fn _ => #" "))
end