data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show,Eq)

--let arv = Node (Leaf 2) 1 (Leaf 3)
-- :t arv

--comtem
occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

--let arv = Node (Leaf 2) 1 (Leaf 3)
--occurs 3 arv
--occurs 0 arv

--transforma em lista
flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r
--que ordem é essa? Pre, Central ou Pos? Central
--let arv = Node (Leaf 2) 1 (Leaf 3)
--flatten arv

--se for uma árvore binária de busca
occursv2 :: Ord a => a -> Tree a -> Bool
occursv2 x (Leaf y) = x == y
occursv2 x (Node l y r) | x == y = True
                        | x < y = occursv2 x l
                        | x > y = occursv2 x r

data ArvBin a = Vazia | Nodo (ArvBin a) a (ArvBin a) deriving (Show,Eq)
--Qual a diferença?
--Reimplemente as mesmas funções
