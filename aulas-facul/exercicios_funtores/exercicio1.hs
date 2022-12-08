--Defina uma instância de Functorpara o seguinte tipo de árvores binárias que possuem dados em seus nodos:
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    --fmap :: (a->b) -> Tree a -> Tree b
    fmap f Leaf = Leaf
    fmap f (Node e x d) = Node (fmap f e) (f x) (fmap f d)
