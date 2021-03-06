{-# OPTIONS_GHC -Wall #-}

--Ex 1: Fibonacci numbers
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

--Ex 2: More Fibonacci numbers
fib2 :: Integer -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = sum $ take 2 (foldr f [0, 1] [0..(n-2)])
  where f _ b = sum (take 2 b) : b

fibs2 :: [Integer]
fibs2 = map fib2 [0..]

--Ex 3: Streams
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  show s = show (take 4 (streamToList s))

--Ex 4: 
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a rs) = Cons (f a) (streamMap f rs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f e = Cons e (streamFromSeed f (f e))

--Ex 5:
nats :: Stream Integer
nats = streamFromSeed (+1) 0

--Cheated
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = (Cons x (interleaveStreams ys xs))
                                   
--The below implementation of interleaveStreams doesn't work for ruler, why?
--interleaveStreams (Cons r rs) (Cons s ss) = (Cons r (Cons s (interleaveStreams rs ss)))

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])
