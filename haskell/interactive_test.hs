read_int = read :: String -> Int
main = interact (unlines . map (show . (foldl (\x y ->  x + read_int y) 0) . words) . lines)
