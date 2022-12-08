multiplic :: Int -> Int -> Int
multiplic _ 0 = 0
multiplic m n = m + multiplic m (n-1)
