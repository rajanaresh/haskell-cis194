{-# OPTIONS_GHC -Wall #-}

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


tag :: Monoid m => JoinList m a -> m
tag (Single m _)                               = mempty `mappend` m
tag (Append _ (JoinList m1 _) (JoinList m2 _)) = tag m1 `mappend` tag m2

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l +++ r = Append (tag l `mappend` tag r) l r
