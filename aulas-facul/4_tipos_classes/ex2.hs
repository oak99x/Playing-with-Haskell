type Pair a = (a, a)

copy :: a -> Pair a
copy x = (x,x)

mult :: Pair Int -> Int
mult (m,n) = m*n
--copy 1
--copy "teste"
