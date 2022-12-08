digitos :: Int -> [Int]
digitos 0 = []
digitos x = digitos (x `div` 10) ++ [x `mod` 10]

armstrong :: Int -> Bool
armstrong x = sum [ v^t | v <- xs ] == x
    where xs = digitos x
          t  = length (digitos x)

getValor :: IO Int
getValor = readLn

main = do
    putStrLn "Digite valor:"
    valor <- getValor
    putStrLn ("Resultado:" ++ show (armstrong valor))