{-# OPTIONS_GHC -Wall #-}

import Data.List ( nub
                 , (\\)
                 )

-- Ex 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr func 1
  where func x y
          | even x    = (x-2)*y
          | otherwise = y


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . (filter even) . (takeWhile (/=0)) . (iterate func)
  where func x
          | x == 1    = 0
          | even x    = x `div` 2
          | otherwise = (3 * x) + 1


--Ex 2: Folding with 
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr f Leaf
  where f e Leaf               = Node 0 Leaf e Leaf
        f e (Node _ l el r)    = case ((count l) <= (count r)) of
          True  -> Node (1 + height (f e l)) (f e l) el r
          False -> Node (1 + height (f e r)) l el (f e r)
        count Leaf             = (0::Integer)
        count (Node _ l _ r)  = (count l) + (count r) + 1 :: Integer
        height Leaf            = 0
        height (Node h _ _ _ ) = h


--Ex 3: More folds!        
xor :: [Bool] -> Bool
xor = foldr (\x y -> ((not x) && y) || (x && (not y))) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> ((f x):y)) []

--Ex 4: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2*x+1 | x <- [1..n]] \\ nub [i+j+2*i*j | i <- [1..n], j <- [1..n]]
