--Uma mônada tem a seguinte declaração:
--class Aplicative m => Monad m where
--    return :: a -> m a
--    (>>=) :: m a -> (a -> m b) -> m b
--    return = pure

--Mônada Maybe
--instance Monad Maybe where
-- -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--    Nothing >>= _ = Nothing
--    (Just x) >>= f = f x

olamundo :: Maybe String
olamundo = do
    x <- Just "Ola "
    y <- Just "Mundo!"
    Just (x ++ y)

--Mônada []
--instance Monad [] where
-- -- (>>=) :: [a] -> (a -> [b]) -> [b]
--    xs >>= f = [y | x <- xs, y <- f x]

paresv1 :: [a] -> [b] -> [(a,b)]
paresv1 xs ys = [(x,y) | x <- xs, y <- ys]
--paresv1 [1,2] [3,4]

paresv2 :: [a] -> [b] -> [(a,b)]
paresv2 xs ys = do
    x <- xs
    y <- ys
    return (x,y)
--paresv2 [1,2] [3,4]

