main = do
    putStrLn "Qual o seu nome?"
    nome <- getLine
    putStrLn ("Oi " ++ nome ++ "!")