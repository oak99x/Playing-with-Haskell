data Tree a = EmptyTree | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap f EmptyTree = EmptyTree
    fmap f (Node esq x dir) = Node (fmap f esq) (f x) (fmap f dir)

--let arv = EmptyTree
--fmap (+1) arv

--let arv = Node (Node EmptyTree 2 EmptyTree) 1 (Node EmptyTree 3 EmptyTree)
--fmap (+1) arv

--let arv = Node EmptyTree "raiz" (Node EmptyTree "folha" EmptyTree)
--fmap length arv

--------------------------------------------------------------------------------
data Tree' a = Leaf a | Node' (Tree' a) (Tree' a)
instance Functor Tree' where
        -- fmap :: (a -> b) -> Tree a -> Tree b
        fmap g (Leaf x)= Leaf (g x)
        fmap g (Node' l r) = Node' (fmap g l) (fmap g r)

--fmap even (Node'(Leaf 1) (Leaf 2)