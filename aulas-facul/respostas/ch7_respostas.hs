--Exercicio 1
--Funções currificadas

--Exercicio 2
exercicio2 :: (a -> b) -> (a -> Bool) -> [a] -> [b]
exercicio2 f p lista = map f (filter p lista)

--Exercicio 3
mapffold :: (a -> b) -> [a] -> [b]
mapffold f = foldr (\x xs -> f x : xs) []

filterfold :: (a -> Bool) -> [a] -> [a]
filterfold p = foldr (\x xs -> if p x then x:xs else xs) []