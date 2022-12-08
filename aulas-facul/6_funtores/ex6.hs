prodsv1 :: [Int] -> [Int] -> [Int]
prodsv1 xs ys = [x * y | x <- xs, y <- ys]

-- prodsv1 [1,2] [3,4]

prodsv2 :: [Int] -> [Int] -> [Int]
-- "estilo aplicativo"
prodsv2 xs ys = pure (*) <*> xs <*> ys

-- prodsv2 [1,2] [3,4]