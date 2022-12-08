--Árvore de expressões aritméticas com operação de divisão
data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

--Como Maybe é uma mônada, podemos reescrever eval usando currying e expressão lambda
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = eval x >>= \n ->
                 eval y >>= \m ->
                 safediv n m

--eval (Div (Val 10) (Val 5))
--eval (Div (Val 10) (Val 0))

--Observe o padrão:
-- m1 >>= \x1 ->
-- m2 >>= \x2 ->
-- ...
-- mn >>= \xn ->
-- f x1 x2 ... xn
-- Ele pode ser escrito de forma mais simples usando notação "do"
-- do x1 <- m1
--    x2 <- m2
--    ...
--    xn <- mn
--    f x1 x2 ... xn

evalv2 :: Expr -> Maybe Int
evalv2 (Val n) = Just n
evalv2 (Div x y) = do
    n <- evalv2 x
    m <- evalv2 y
    safediv n m

--evalv2 (Div (Val 10) (Val 5))
--evalv2 (Div (Val 10) (Val 0))