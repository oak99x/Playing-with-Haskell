--sequenceA :: (Applicative f) => [f a] -> f [a]  
--sequenceA [] = pure []  
--sequenceA (x:xs) = pure (:) <*> x <*> sequenceA xs

--sequenceA [Just 3, Just 2, Just 1]
--sequenceA [Just 3, Nothing, Just 1]
--sequenceA [(+3),(+2),(+1)] 3
--sequenceA [[1,2],[3,4]]

getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar)
