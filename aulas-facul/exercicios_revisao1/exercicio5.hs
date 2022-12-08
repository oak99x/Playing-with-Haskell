replicate' :: Int -> a -> [a]
replicate' n e = [e | _ <- [1..n]]
