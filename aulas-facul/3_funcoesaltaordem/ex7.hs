reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

reversofoldr :: [a] -> [a]
reversofoldr = foldr (\x xs -> xs ++ [x]) []

