-- Mostre que a função currificada a seguirpode ser formalizada através de expressões lambda:
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

mult' :: Int -> (Int -> (Int -> Int))
mult' = \x -> (\y -> (\z -> x * y * z))