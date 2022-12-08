-- Qual o significado de currying usando o mapeamento de uma função com dois parâmetros sobre um funtor?
-- :t fmap (+) 1
-- Obtemos um funtor com uma função "dentro"

-- Qual é o resultado?
-- let a = fmap (*) [1,2,3]
-- :t a
-- fmap (\f -> f 2) a

-- Mas como mapear uma função que está dentro de um funtor sobre outro funtor?
-- Com fmap isso não é possível
-- Precisamos de funtores aplicativos!