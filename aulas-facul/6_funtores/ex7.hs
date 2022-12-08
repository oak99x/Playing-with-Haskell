-- Exemplo IO
-- O uso do funtor aplicativo implementa uma noção de "sequência" de ações

acaov1 :: IO String
acaov1 = do
    a <- getLine
    b <- getLine
    return (a ++ b)

acaov2 :: IO String
acaov2 = pure (++) <*> getLine <*> getLine
