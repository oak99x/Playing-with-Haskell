and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False = False
            | otherwise = and' xs

concat' :: [[a]] -> [a]
concat' []     = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n-1) x

elem' :: Eq a =>  a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) | e == x = True
               | otherwise = elem' e xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]     -- lista com um elemento apenas
msort xs = merge (msort(left)) (msort(right))
    where
        left  = take (length xs `div` 2) xs
        right = drop (length xs `div` 2) xs
