-- Exercicio 1
--Uma tripla(x,y,z) de inteirospositivosé chamadapitagóricase x2+ y2= z2. 
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Exercicio 2
--Um inteiropositivoé ditoperfeitose elefor iguala soma de todososseusdivisores, com exceçãodo próprionúmero.
perfect :: Int -> Bool
perfect n = sum [x | x <- [1..n-1], n `mod` x == 0] == n

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]

-- Exercicio 3
--O produtoescalarde duaslistasde inteirosxse ys
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]
