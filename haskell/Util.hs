{-# LANGUAGE OverloadedStrings #-}
module Util where
import Data.List
--- Example of infix operators
-- same precedence as elem and notElem
infixl 4 ∈,∉
(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
(∈) = elem
(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
(∉) = notElem
(%) = mod
(./.) = div
-- ordered set difference
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
-- fairly inefficent prime number sieve
primes n = eratos [2..n]
  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..n])

factor :: Integer -> [Integer]
factor n = let ps = primes (ceiling ((fromIntegral n)**0.5))
               hd_or_0 ls = if null ls then 0 else head ls
               acc 1 ps fs = fs
               acc n [] fs = n:fs
               acc n (p:ps) fs
                  | n < (p*p) = n:fs
                  | otherwise =
                      if (n % p) == 0 then
                         acc (n ./. p) (p:ps) (p:fs)
                      else acc n ps fs in
     acc n ps []

listToTuple2 [a,b] = (a,b)
listToTuple3 [a,b,c] = (a,b,c)
listToTuple4 [a,b,c,d] = (a,b,c,d)

zipSelf :: [a] -> [(a,a)]
zipSelf ls =
  let acc [] ys = reverse ys
      acc [x] ys = reverse ys
      acc (x0:x1:xs) ys = acc xs ((x0,x1):ys) in
  acc ls []

toDigits x = let acc 0 ls = ls
                 acc n ls = acc (n `div` 10) ((n `mod` 10):ls) in
    acc x []
mostFrequentDigitSum :: Int -> Int
mostFrequentDigitSum n =
    let step x = x - sum(toDigits(x))
        ls = takeWhile (\x -> x/=0) $ iterate step n
        ls' = map step ls
        groups = group $ sortBy (\x y -> compare y x) ls'
        groups' = sortBy (\x y -> compare (length y) (length x)) groups in
    head $ head groups'
arrayConversion x =
    let acc _ [x] = x
        acc True ls = acc (*) $ map (\(x,y) -> x+y) $ zipSelf ls
        acc False ls = acc (+) $ map (\(x,y) -> x*y) $ zipSelf ls in
    acc True x
-- toDigits x = let acc 0 ls = ls
--                  acc n ls = acc (n // 10) ((n % 10):ls)
-- --- Example of data structures
-- -- Functional queue
-- data Queue a = [a] [a] deriving (Read, Show)

-- enqueue :: Queue a -> a -> Queue a
-- enqueue_list :: Queue a -> [a] -> Queue a
-- dequeue :: Queue a -> (a, Queue a)
-- peek :: Queue a -> Maybe a

-- enqueue (Queue [] []) x = Queue [] x
-- enqueue (Queue e d) x = Queue (x:e) d
-- -- iterate f a --> [a,f(a),f(f(a)),...]
-- enqueue_list (Queue [] []) xs = Queue [] (reverse xs)
-- enqueue_list (Queue e d) xs = Queue (xs++e) d

-- dequeue (Queue [] []) = error "Can't dequeue from a empty queue"
-- -- To support peek we need to make sure there's always a value
-- -- in the dequeue list, unless the queue is empty
-- dequeue (Queue e (d:d2:ds)) = (d,(Queue e (d2:ds)))
-- dequeue (Queue [] (d:ds)) = (d, Queue [] ds)
-- dequeue (Queue e d) = dequeue $ Queue [] (d++(reverse e))

-- peek Queue

-- -- Simple binary tree
-- data Tree a = a (Tree a) (Tree a) | a

-- bfs :: Tree a -> (a -> b -> b) -> b
