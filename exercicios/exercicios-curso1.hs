import Data.Map (deleteAt)
-- :: (é do tipo)
--ex 
-- False :: Bool

--pega o tamanho da lista
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:xs) = 1 + tamanho xs

impar :: Integral a => a -> Bool
impar n = n `mod` 2 == 1

quadrado :: Num a => a -> a
quadrado n = n * n

eq2grau :: Floating a => a -> a -> a -> (a,a)
eq2grau a b c = (((-b) + sqrt (b^2 -4 * a * c)) / (2 * a),
                 ((-b) - sqrt (b^2 -4 * a * c)) / (2 * a))

eq2grauV2 :: Floating a => a -> a -> a -> (a,a)
eq2grauV2 a b c = (x1,x2)
        where
            x1 = ((-b) + sqDelta) / (2 * a)
            x2 = ((-b) - sqDelta) / (2 * a)
            sqDelta = sqrt delta
            delta = b^2 -4 * a * c

--Condicionais, Guards e Pattern Matching
abs :: (Num a, Ord a) => a -> a
abs n = if (n < 0) then (-n) else n

signum1 :: Integral  a => a -> a
signum1 n = if (n < 0) then -1
            else if (n == 0) then 0
            else 1

signum2 :: Integral  a => a -> a
signum2 n  | n < 0  = -1
            | n == 0 = 0
            | otherwise = 1

mult :: (Eq p, Num p) => p -> p -> p
mult 0 _ = 0                            -- _ (qualquer coisa)
mult _ 0 = 0
mult x y = x * y


--Listas
index1 :: [a] -> Int -> a
index1 xs i = head (drop i xs)

fatorial :: Integer -> Integer
fatorial n = product [2..n]

--Compreensão de Listas

--[ expressão(resultado final) | expressao geradora , filtro]
--[ x | x <- [0,2..100], x `mod`6 == 0] --encontra os multiplos de 6

--pares até n ao quadrado
multpares :: Integral a => a -> [a]
multpares n = [ x * 2 | x <- [0..n], even x]

--concatena em uma so lista uma lista de listas
minhaconcat :: [[a]] -> [a]
minhaconcat xss = [ x | xs <- xss, x <- xs]

--tamanho da lista versao 2
tam :: Num a => [t] -> a
tam xs = sum [1 | _ <- xs]

divisores :: Integral a => a -> [a]
divisores n = [ x | x <-[1..n], (n `mod` x) == 0]

primo :: Integral x => x -> Bool
primo n = if (tam (divisores n)) == 2 then True else False

primo2 :: Integral x => x -> Bool
primo2 n = divisores n == [1,n]

listaprimos :: Integral a => a -> [a]
listaprimos n = [ x | x <- [1..n], primo x]

reverse2 :: [a] -> [a]
reverse2 xs = reverse xs

-----------------------------------------------------------------------------
--Recursão simples e em listas
-----------------------------------------------------------------------------

reverse3 :: [a] -> [a]
reverse3 [] = []
reverse3 (x:xs) = (reverse3 xs)++[x]

sorted :: [Int] -> Bool
sorted [] = True
sorted xs = null [ ()| (x, y) <- (zip xs(tail xs)), x > y]

fatorial2 :: Integer -> Integer
fatorial2 0 = 1
fatorial2 n = n * fatorial2 (n-1)

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

somaLista :: Num a => [a] -> a
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

tamanho2 :: [a] -> Int
tamanho2 [] = 0
tamanho2 (_:xs) = 1 + tamanho2 xs

produtoLista :: Num a => [a] -> a
produtoLista [] = 1
produtoLista (x:xs) = x * produtoLista xs


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) =  qsort menores ++ [x] ++ qsort maiores
                where
                    menores = [e | e <- xs, e < x]
                    maiores = [e | e <- xs, e >= x]

concat' :: [a] -> [a] -> [a]
concat' [] ys = ys
concat' (x:xs) ys = x : concat' xs ys

-----------------------------------------------------------------------------
--6.1 - Programação Funcional em Haskell: Funções de Alta Ordem
--funçoes que recebem funcoes
-----------------------------------------------------------------------------

duasvezes :: (a -> a) -> a -> a
duasvezes f x = f (f x)
--exemplo
-- duasvezes (*2) 3              resultado 12

quadruplica :: Integer -> Integer
quadruplica = duasvezes (*2)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
--ex map' (*2) [1,2,3]           resultado [2,4,6]

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [ x | x <- xs, f x]

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f [] = []
filter'' f (x:xs)   | f x = x : filter'' f xs
                    | otherwise = filter'' f xs

somaQuadPares :: [Int] -> Int
somaQuadPares xs = sum [ n^2 | n <- xs, even n]

somaQuadPares' :: [Int] -> Int
somaQuadPares' [] = 0
somaQuadPares' xs = sum (map'' (^2) (filter'' even xs))

--com pipe
somaQuadPares'' :: [Int] -> Int
somaQuadPares'' [] = 0
somaQuadPares'' xs = sum 
                     $ map'' (^2) 
                     $ filter'' even xs

-----------------------------------------------------------------------------
--Dobrando sua lista: foldr, foldl, foldl'
-----------------------------------------------------------------------------
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

produto2 :: Num a => [a] -> a
produto2 = foldr' (*) 1

tamanho3 :: [a] -> Int
tamanho3 = foldr' (\_ n -> n+1) 0

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
reverse4 :: [a] -> [a]
reverse4 = foldr' snoc []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f v [] = v
foldl' f v (x:xs) = foldl' f (f v x) xs


-----------------------------------------------------------------------------
--Composição de Funçoes
-----------------------------------------------------------------------------
--(f . g) x = f (g x)
fn x = (ceiling . negate  . tan . cos . max 50) x


-----------------------------------------------------------------------------
--Criando apelidos de tipos com type
-----------------------------------------------------------------------------
type Produto = (Integer, String, Double)
type Cliente = (Integer, String, Double)

troco :: Produto -> Cliente -> Double
troco (idprod, nome, preco) (idcli, nomeCli, pago) = pago - preco

--ex troco (1,"bolo", 100) (1,"Lucas", 150)     resultado 50.0

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k xs = head [ v | (k',v) <- xs, k' == k]

-----------------------------------------------------------------------------
--Tipos de Dados Algébricos
-----------------------------------------------------------------------------
--Tipo Soma
data Dir = Norte | Sul | Leste | Oeste  deriving Show

type Coord = (Int, Int)
type Passo = Coord -> Coord

para :: Dir -> Passo --igual a Dir -> Coord -> Coord
para Norte (x,y) = (x,y+1)
para Sul (x,y) = (x,y-1)
para Leste (x,y) = (x+1,y)
para Oeste (x,y) = (x-1,y)

caminhar :: [Dir] -> Passo
caminhar [] coord = coord
caminhar (d:ds) coord = caminhar ds (para d coord)
--caminhar (d:ds) coord = foldl (flip para) coord ds

--Tipo Produto
data Ponto = MkPonto Double Double deriving Show
--também pode ser =>> data Ponto = Ponto Double Double

dist :: Ponto -> Ponto -> Double
dist (MkPonto x y) (MkPonto x' y') = sqrt ((x - x')^2 + (y - y')^2)

--Tipo parametrizados
data Identidade a = Id a deriving Show

-- data Maybe a = Nothing | Just a

maybeDiv :: Int -> Int -> Maybe Int
maybeDiv _ 0 = Nothing
maybeDiv x y = Just (x `div` y)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

-- data Either a b = Left a | Right b

eitherDiv :: Int -> Int -> Either String Int
eitherDiv _ 0 = Left "divisao por 0"
eitherDiv x y = Right (x `div` y)

data Fuzzy = Verdadeiro | Falso | Pertinencia Double deriving Show

fuzzifica :: Double -> Fuzzy
fuzzifica x | x <= 0 = Falso
            | x >= 1 = Verdadeiro
            | otherwise = Pertinencia x

--Tipos de Dados Recursivos
data Lista a = Vazia | Cons a (Lista a)
--Cons 1 (Cons 2 (Cons 3 Vazia))
--1 : 2 : 3 : []

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

arv :: Tree Int
arv =  Node a1 4 a2 
a1 = Node (Leaf 1) 2 (Leaf 3)
a2 = Node (Leaf 5) 6 (Leaf 7)

contem :: Eq a => Tree a -> a -> Bool
contem (Leaf y) x = x == y
contem (Node l y r) x = x == y || contem l x || contem r x 

contem' :: Ord a => Tree a -> a -> Bool
contem' (Leaf y) x = x == y
contem' (Node l y r) x  | x == y = True
                        | x < y  = contem l x
                        | x > y  = contem r x 

-----------------------------------------------------------------------------
--Estrutura Zipper
-----------------------------------------------------------------------------