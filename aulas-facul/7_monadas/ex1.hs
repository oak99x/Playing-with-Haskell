--Árvore de expressões aritméticas com operação de divisão
data Expr = Val Int | Div Expr Expr

eval :: Expr -> Int
eval (Val n) = n
eval (Div x y) = eval x `div` eval y

--eval (Div (Val 10) (Val 5))
--eval (Div (Val 10) (Val 0))
--resulta em uma exceção de divisão por zero