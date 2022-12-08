data Shape = Circle Float | Rectangle Float Float deriving (Show)
--Circle raio
--Rectangle base altura

-- :t Circle

square :: Float -> Shape
square n = Rectangle n n

--square 3
--map (Rectangle 3) [1,2,3]

area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle b h) = b*h

--area (Circle 2)
--area (square 3)
