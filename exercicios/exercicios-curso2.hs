import Distribution.Compat.Lens (_1)
--Somando números
somatorio :: Int -> Int
somatorio 0 = 0
somatorio n = n + somatorio(n-1)

--fatorial
fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial(n-1)

--fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

--guarda |
guarda :: (Ord a, Num a) => a -> Bool
guarda x    | x > 0 = True
            | x < 0 = False
            | otherwise = False

sizeList :: [a] -> Int
sizeList [] = 0
sizeList (x:xs) = 1 + sizeList(xs)


compareList :: Eq a => [a] -> [a] -> Bool
compareList [] [] = True
compareList [] _ = False
compareList _ [] = False
compareList (x:xs) (y:ys)   | (x == y) = compareList xs ys
                            | otherwise = False
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

pertence :: Eq a => [a] -> a -> Bool
pertence [] _ = False
pertence (x:xs) n   | (x == n) = True
                    | otherwise = pertence xs n

maior :: Ord a => [a] -> a
maior [x] = x
maior (x:xs) | (x > maior xs) = x
             | otherwise = maior xs

--função zip combina pares
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

--qsort
ordList :: Ord a => [a] -> [a]
ordList [] = []
ordList (x:xs) =  ordList menores ++ [x] ++ ordList maiores
                where
                    menores = [e | e <- xs, e < x]
                    maiores = [e | e <- xs, e >= x]

