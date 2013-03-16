fun qsort << xs = let
    fun qs [] = []
      | qs [x] = [x]
      | qs (p::xs) = let
          val comp = (fn x => << (x,p))
      in
          qs (List.fliter comp xs) @ p :: (qs (List.filter (not o comp) xs))
      end
in
    qs xs
end
