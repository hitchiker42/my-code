structure hw6 =
struct
fun quicksort filt (x::xs) = let
    val f = List.filter
    fun qs [] = []
      | qs (x::[]) = (x::[])
      | qs (x::xs) = let
          val pred = fn y => filt (y,x)
      in qs (f pred xs)@x::(qs (List.filter (not o pred) xs)) end
in qs (x::xs) end
end
