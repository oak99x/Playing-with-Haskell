--map (+1) [1,2,3]
--map even [1,2,3,4]
--map (replicate 3) [3..6]
--map (map (^2)) [[1,2],[3,4,5,6]]

mapv1 :: (a->b) -> [a] -> [b]
mapv1 f xs = [f x | x <- xs]

mapv2 :: (a->b) -> [a] -> [b]
mapv2 f [] = []
mapv2 f (x:xs) = f x : map f xs
