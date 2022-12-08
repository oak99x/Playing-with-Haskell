--Funções que retornam funções como resultado
somar :: Int -> Int -> Int
somar x y = x + y

--let s = somar 2
--s 3

somarv2 :: Int -> (Int -> Int)
somarv2 = \x -> (\y -> x + y)

--Funções que recebem funções como parâmetros
twice :: (a -> a) -> a -> a
twice f x = f (f x)

--twice (+2) 3
--twice reverse [1,2,3]


