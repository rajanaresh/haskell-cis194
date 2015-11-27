{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List ( group
                 , sort
                 , transpose
                 , any
                 )

-- Skips
-- Recursive solution
skips :: [a] -> [[a]]
skips l = take (length l) (map (\x -> skip x x l) [0..])

skip :: Int -> Int -> [a] -> [a]
skip _ _ [] = []
skip orig n (x:xs)
  | n == 0    = x : (skip orig orig xs)
  | otherwise = skip orig (n-1) xs

-- Non-recursive solution
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

skip' :: Int -> [a] -> [Maybe a]
skip' n l
  | n /= 0    = func [n,(2*n)..]
  | otherwise = func [0,1..]
  where func          = (take e . (map (\x -> safeHead $ drop x l)))
        e | n == 0    = length l
          | otherwise = ((length l)-1) `div` n

skips' :: [a] -> [[a]]
skips' l = take (length l) (map
                            (\x -> (map (\(Just y) -> y) (skip' x l)))
                            [0..])

-- Local maximum
localMaxima :: [Integer] -> [Integer]
localMaxima []     = []
localMaxima [_]    = []
localMaxima [_,_]  = []
localMaxima (x:y:z:rs)
  | x < y && y > z = [y] ++ localMaxima (z:rs)
  | otherwise      = localMaxima(y:z:rs)


histogram :: [Integer] -> String
histogram l = foldr (++) "" ((map (\x-> (map func x)++"\n") result) ++ postfix)
  where d          = reverse $ transpose (group $ sort l)
        result     =  map
                      (\y -> map
                             (\z -> any (\x -> x == z) y)
                             [0..9])
                      d
        func False = ' '
        func True  = '*'
        postfix    = ["==========\n"]++["0123456789\n"]
                             

