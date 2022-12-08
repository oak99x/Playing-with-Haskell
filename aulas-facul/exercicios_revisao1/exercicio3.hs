-- Defina a função third :: [a] -> aque retorna o terceiro elemento de 
-- uma lista que contenha no mínimo três elementos usando:

-- versão a
third' :: [a] -> a
third' xs = head (tail (tail xs))
--versão b
third'' :: [a] -> a
third'' xs = xs !! 2
--versão c
third''' :: [a] -> a
third''' (_:_:x:_) = x

thirdv :: [a] -> a
thirdv xs = head (drop 2 xs)
