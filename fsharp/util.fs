module Util
let fact n =
    let rec loop n acc =
        match n with
            | 0 -> acc
            | n -> loop (n-1) (n*acc)
    in
        loop n 1
    ;;
    
