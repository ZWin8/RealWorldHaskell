import Control.Monad

data Foo e a = L e | R a deriving (Show)
instance Functor (Foo e)  where
         fmap _ (L e) = L e
         fmap g (R a) = R (g a)

-- instance Functor ((->) e) where
--         fmap g ((->) e a) = (->) e (g a)

-- instance Functor ((,) e) where
--         fmap g (e, a) = (e, g a)

data Pair a = Pair a a deriving (Show)
instance Functor Pair where
     fmap g (Pair x y) = Pair (g x) (g y)

data ITree a = Leaf (Int->a)
               | Node (ITree a)

instance Functor ITree where
         fmap g (Leaf f) = Leaf (g.f)
         fmap g (Node t) = Node (fmap g t)
instance Show (ITree a) where
         show (Leaf _) = " Leaf "
         show (Node t) = " Node " ++ show t
