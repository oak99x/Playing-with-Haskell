import Data.Char  
  
main = do  
    putStrLn "Qual seu primeiro nome?"  
    primeiroNome <- getLine  
    putStrLn "Qual seu último nome?"  
    ultimoNome <- getLine  
    let mPrimeiroNome = map toUpper primeiroNome  
        mUltimoNome = map toUpper ultimoNome  
    putStrLn ("Oi " ++ mPrimeiroNome ++ " " ++ mUltimoNome ++ ", como vai você?")