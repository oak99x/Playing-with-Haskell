--filter even [1..10]
--filter (>3) [1..10]
--filter (\xs -> not (null xs)) [[1,2],[],[1],[],[]]

filterv1 :: (a -> Bool) -> [a] -> [a]
filterv1 p xs = [x | x <- xs, p x]

filterv2 :: (a -> Bool) -> [a] -> [a]
filterv2 p [] = []
filterv2 p (x:xs) | p x = x : filterv2 p xs
                  | otherwise = filterv2 p xs