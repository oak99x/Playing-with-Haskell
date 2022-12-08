data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
    --fmap :: (a->b) -> Tree a -> Tree b
    fmap f (Leaf n) = Leaf (f n)
    fmap f (Node e d) = Node (fmap f e) (fmap f d)
