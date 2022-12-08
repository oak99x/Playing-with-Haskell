euclid :: Int -> Int -> Int
euclid a b  | b == 0 = a
            | otherwise = euclid b (a `mod` b)

euclid' :: Int -> Int -> Int
euclid' a b  = if b == 0 then a else euclid' b (a `mod` b)
