--Defina uma instância de Applicativepara o tipo (a -> ):
instance Applicative ((->) a) where
--pure :: a -> f a
--pure :: b -> (a -> b)
pure = const
-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: (a->b->c) -> (a->b) -> (a->c)
g <*> h = \x -> g x (h x)
