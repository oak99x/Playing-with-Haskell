--Árvore de expressões aritméticas com operação de divisão
data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

--Podemos estar interessados em usar funtores aplicativos para simplificar a definição de eval
--Mas isso não funciona...
eval :: Expr -> Maybe Int
eval (Val n) = pure n
--eval (Div x y) = pure safediv <*> eval x <*> eval y
-- O problema está no tipo de safediv que é Int -> Int -> Maybe Int mas para o funtor aplicativo
-- funcionar deveria ser Int -> Int -> Int
-- A solução é usar mônadas!

--Uma mônada possui um operador >>=, chamado de bind, que abstrai o padrão de computação do exemplo
--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--mx >>= f = case mx of
--    Nothing -> Nothing
--    Just x -> f x
-- Interpretação: >>= toma um argumento do tipo a que pode falhar e uma função a -> b
-- cujo resultado pode falhar, e retorna um resultado do tipo b que pode falhar.