-- exercício 1
safetailA :: [a] -> [a]
safetailA xs = if null xs then [] else tail xs

safetailB:: [a] -> [a]
safetailB xs | null xs   = []
             | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC []     = []
safetailC (_:xs) = xs

-- exercicio 2
or' False False = False
or' False True  = True
or' True  False = True
or' True  True  = True

or'' False False = False
or'' _     _     = True
 
or''' False  b   = b
or''' True   _   = True

or'''' a b  | a == b = a
or'''' _ _           = True

-- exercicio 3
and' :: Bool -> Bool -> Bool
and' a b = if a && b
        then True
        else False

-- exercício 4
and'' :: Bool -> Bool -> Bool
and'' a b = if a
        then b
        else False
