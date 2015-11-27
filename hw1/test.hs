toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 0 || n == 0 = []
  | n > 0 = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ns = reverse (doubleEveryOtherAux (encode ns))
  where encode ns = zip [1..] (reverse ns)

doubleEveryOtherAux [] = []
doubleEveryOtherAux ((i,x):ns)
  | even i = 2*x : doubleEveryOtherAux ns
  | otherwise = x : doubleEveryOtherAux ns
            
sumDigits :: [Integer] -> Integer
sumDigits ns = sum (foldr (++) [] (map toDigits ns))

validate :: Integer -> Bool
validate n
  | m  `mod` 10 == 0 = True
  | otherwise = False
  where m = sumDigits (doubleEveryOther $ toDigits n)
