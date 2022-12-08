-- padrão de aplicação de uma função sobre elementos de uma estrutura

-- Função para incrementar os elementos de uma lista de inteiros
inc :: [Int] -> [Int]
inc [] = []
inc (n:ns) = n+1 : inc ns
--inc [1,2,3]

-- Função map generaliza esse padrão de aplicação de uma função sobre listas
-- map :: (a -> b) -> [a] -> [b]
incv2 :: [Int] -> [Int]
incv2 = map (+1)
--incv2 [1,2,3]

-- Podemos generalizar essa noção de aplicação de uma função sobre elementos de uma estrutura
-- sobre diferentes tipos parametrizados através do conceito de funtor
-- fmap :: (a -> b) -> f a -> f b

-- map é um funtor que trabalha sobre listas! Lista é uma instância da typeclass Functor
-- instance Functor [] where  
--    fmap = map  

incv3 :: [Int] -> [Int]
incv3 = fmap (+1)

incv4 :: Functor f => f Int -> f Int
incv4 = fmap (+1)

-- incv4 agora é ma função genérica que trabalho sobre qualquer tipo funtorial
-- incv4 [1,2,3]