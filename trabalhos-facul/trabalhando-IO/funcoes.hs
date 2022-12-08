import Data.Char

type CIN = String

-- Exercício 1 - (palind)
palind :: IO ()
palind = do putStr "Digite uma cadeia de caracteres: "
            line <- getLine
            do if (palindromo line) then
                putStr "é um palindromo\n" else
                putStr "não é um palindromo\n"
            return ()

palindromo :: String -> Bool
palindromo str = (formataString str == (reverse (formataString str)))

formataString :: String -> String
formataString "" = ""
formataString (x:xs) = if x == ' ' || (isPunctuation x) then "" ++ formataString xs else [(toLower x)] ++ formataString xs


-- Exercício 2 - (addSum)
addSum :: CIN -> CIN
addSum cin = verifica cin

verifica :: CIN -> CIN
verifica str = if length str == 8 then str ++ (show (somaCIN str)) else "CIN inválido, deve conter 8 digitos."

somaCIN :: CIN -> Int
somaCIN "" = 0
somaCIN (x:xs) = getDigit x + somaCIN xs

getDigit :: Char -> Int
getDigit c = read [c]

-- Exercício 2 - validação - (validar)
validar :: CIN -> Bool
validar cin = somaCIN (take 8 cin) == read (retorna_soma_concatenada cin 8)

retorna_soma_concatenada :: CIN -> Int -> CIN
retorna_soma_concatenada cin 0 = cin
retorna_soma_concatenada (x:xs) i = retorna_soma_concatenada xs (i-1)



-- Exercício 3 - (strlen)
strlen :: IO ()
strlen = do putStr "digite a quantidade de números que deseja somar: "
            line <- getLine
            sum <- sumints ((read line))
            putStr (show sum)
            return ()

getInt :: IO Int          
getInt = do  
  s <- getLine
  return (read s :: Int)

sumints :: Int -> IO Int
sumints 0 = return (0)
sumints i = do
            x<-getInt
            sum <- sumints (i - 1)
            return (x + sum)