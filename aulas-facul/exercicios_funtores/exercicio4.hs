--Dado o seguinte tipo para expressões aritméticas  
--data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show
--Mostre como tornar esse tipo uma instância de Functore Applicative.

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    --fmap :: (a->b) -> Expr a -> Expr b
    fmap f (Var x) = Var (f x)
    fmap f (Val n) = Val n
    fmap f (Add e d) = Add (fmap f e) (fmap f d)

instance Applicative Expr where
    --pure :: a -> Expr a
    pure = Var
    --(<*>) :: Expr (a->b) - Expr a -> Expr b
    (Var g) <*> ex = fmap g ex
    (Val n) <*> ex = Val n
    (Add e d) <*> ex = Add (e <*> ex) (d <*> ex)
