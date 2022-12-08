import Text.Printf

fazAlgo :: Double -> Double
fazAlgo n = n * n

getDouble :: IO Double
getDouble = readLn

main = do
    putStrLn "Digite valor ponto flutuante:"
    --valor <- getLine
    --let valordouble = (read valor :: Double)
    valordouble <- getDouble
    printf "%.2f" valordouble