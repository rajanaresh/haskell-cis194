{-# OPTIONS_GHC -Wall #-}

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag (Single m a) = 

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a

