--IO também é uma instância de Functor
--instance Functor IO where  
--    fmap f action = do  
--        result <- action  
--        return (f result)

main = do
    line <- fmap reverse getLine
    putStrLn ("Você digitou " ++ line ++ " ao reverso")