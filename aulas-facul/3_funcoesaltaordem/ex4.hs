somatorio :: Num a => [a] -> a
somatorio [] = 0
somatorio (x:xs) = x + somatorio xs

-- somatorio [1,2,3]
-- somatorio 1:2:3:[]
-- = 1 + somatorio [2,3]
-- = 1 + (2 + somatorio [3])
-- = 1 + (2 + (3 + somatorio []))
-- = 1 + (2 + (3 + 0))

--fold também conhecida (em outras linguagens) como reduce é uma função de redução
--ela analisa uma estrutura de dados recursiva aplicando um operador de combinação sobre os dados

somatoriofoldr :: Num a => [a] -> a
somatoriofoldr = foldr (+) 0

-- somatorio 1:2:3:[]
-- foldr (+) 0 1:2:3:[]
-- 1 + (2 + (3 + 0))