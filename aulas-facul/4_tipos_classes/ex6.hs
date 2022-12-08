data Nat = Zero | Succ Nat deriving (Show, Eq)

--Zero
--Succ Zero

-- Nat para Int
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

--nat2int (Succ (Succ Zero))

-- Int para Nat
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

--int2nat 3

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

--add (Succ Zero) (Succ Zero)

add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)