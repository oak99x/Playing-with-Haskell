--Árvore de expressões aritméticas com operação de divisão
data Expr = Val Int | Div Expr Expr

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
    Nothing -> Nothing
    Just n -> case eval y of
        Nothing -> Nothing
        Just m -> safediv n m

--eval (Div (Val 10) (Val 5))
--eval (Div (Val 10) (Val 0))
--resultado é Nothing, que interpretamos como uma falha

