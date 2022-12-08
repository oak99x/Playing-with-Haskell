data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)
 
size :: Expr -> Int
size (Val n) = 1
size (Add x y) = size x + size y
size (Mul x y) = size x + size y

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

-- let e = Add (Val 1) (Mul (Val 2) (Val 3))
-- size e
-- eval e
 