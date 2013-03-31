fun natList 0 = []
  | natList 1 = 1::[]
  | natList n = let
(*tail recursion I think*)
      fun acc 0 r = r
        | acc n r = acc (n-1) (n::r)
  in rev (acc n []) end