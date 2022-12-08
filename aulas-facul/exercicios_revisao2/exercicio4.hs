data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    --fmap :: (a->b) -> Expr a -> Expr b
    fmap f (Var x) = Var (f x)
    fmap f (Val n) = Val n
    fmap f (Add e d) = Add (fmap f e) (fmap f d)

instance Applicative Expr where
    --pure :: a -> Expr a
    pure = Var
    --(<*>) :: Expr (a->b) -> Expr a -> Expr b
    (Var g) <*> ex = fmap g ex
    (Val n) <*> ex = Val n
    (Add e d) <*> ex = Add (e <*> ex) (d <*> ex)

instance Monad Expr where
    --(>>=) :: Expr a -> (a -> Expr b) -> Expr b
    (Var x) >>= g = g x
    (Val n) >>= g = Val n
    (Add e d) >>= g = Add (e >>= g) (d >>= g)

-- O operador >>= implementa o conceito de substituição de variáveis
-- em uma expressão aritmética, na qual uma variável é substituída por outra expressão.
-- let e = Add (Val 1) (Var 'x')
-- let g 'x' = Val 2
-- e >>= g
