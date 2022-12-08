-- Composição de funções
-- f(g(x)) = (f . g)(x)

par :: Int -> Bool
par n = n `rem` 2 == 0

impar :: Int -> Bool
impar = not . par
