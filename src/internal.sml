structure Internal = struct
  structure Util = struct
    structure List = struct
      open List

      fun foldr1 f xs =
        case xs of
          [] => raise Fail "Empty list passed to foldr1"
        | [x] => x
        | x :: xs' => f (x, foldr1 f xs')
    end

    fun spaces n = String.implode (List.tabulate (n, fn _ => #" "))
  end
end