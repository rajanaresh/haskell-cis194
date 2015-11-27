{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage m
  | tp == ["E"] = LogMessage (Error  (read ts :: Int)) (read ets :: Int) (unwords (drop 3 w))
  | tp == ["I"] = LogMessage Info (read ts :: Int) msg
  | tp == ["W"] = LogMessage Warning (read ts :: Int) msg
  | otherwise   = Unknown m
  where w   = words m
        tp  = take 1 w
        ts  = unwords (drop 1 (take 2 w))
        msg = unwords (drop 2 w)
        ets = unwords (drop 2 (take 3 w))

parse :: String -> [LogMessage]
parse l = map parseMessage (lines l)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert l@(LogMessage _ ts _) t = case t of
  Leaf          -> Node Leaf l Leaf
  -- Non-exhaustive pattern Node _ (Unknown _) _ below: 
  Node left lm@(LogMessage _ tts _ ) right                   
    | ts < tts  -> Node (insert l left) lm right
    | otherwise -> Node left lm (insert l right)

build :: [LogMessage] -> MessageTree
build lms = foldr (\x y -> insert x y) Leaf lms

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = map mf (filter ff (inOrder (build lms)))
  where ff x = case x of
          Unknown _              -> False
          LogMessage (Error ec) _ _
            | ec > 50            -> True
            | otherwise          -> False
          LogMessage Info _ _    -> False
          LogMessage Warning _ _ -> False
        mf x = case x of
          Unknown _              -> ""
          LogMessage _ _ msg     -> msg
