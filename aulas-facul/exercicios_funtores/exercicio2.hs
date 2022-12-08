--Complete a seguinte declaração de modo a tornar a função parcialmente aplicada (a -> ) em um funtor:
instance Functor ((->) a) where
    --fmap :: (b->c) -> (a->b) -> (a->c)
    fmap = (.)
