{-# OPTIONS_GHC -Wall #-}

-- Credit card validation
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = (n `mod` 10) : toDigitsRev (n `div` 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = reverse (doubleEveryOtherAux (encode ns))

encode :: [Integer] -> [(Integer, Integer)]
encode ns = zip [1..] (reverse ns)

doubleEveryOtherAux :: [(Integer,Integer)] -> [Integer]
doubleEveryOtherAux [] = []
doubleEveryOtherAux ((i,x):ns)
  | even i    = 2*x : doubleEveryOtherAux ns
  | otherwise = x   : doubleEveryOtherAux ns
            
sumDigits :: [Integer] -> Integer
sumDigits ns = sum (foldr (++) [] (map toDigits ns))

validate :: Integer -> Bool
validate n
  | m  `mod` 10 == 0 = True
  | otherwise        = False
  where m = sumDigits (doubleEveryOther $ toDigits n)

-- Tower of hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n f s t
  | n == 0    = []
  | n == 1    = [(f, s)]
  | otherwise = (hanoi (n - 1) f t s) ++ [(f, s)] ++ (hanoi (n-1) t s f)
