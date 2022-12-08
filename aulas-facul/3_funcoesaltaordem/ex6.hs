e :: [Bool] -> Bool
e [] = True 
e (x:xs) = x && e xs

efoldr :: [Bool] -> Bool
efoldr = foldr (&&) True
