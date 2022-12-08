tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

-- tamanho [1,2,3]
-- tamanho 1:2:3:[]
-- = 1 + tamanho 2:3:[]
-- = 1 + (1 + tamanho 3:[])
-- = 1 + (1 + (1 + tamanho []))
-- = 1 + (1 + (1 + 0))

tamanhofoldr :: [a] -> Int
tamanhofoldr = foldr (\x acc -> 1 + acc) 0
