getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)

--getChars 0
--getChars 1
--getChars 2

main = getChars 2