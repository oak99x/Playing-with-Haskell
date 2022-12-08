type Pos = (Int,Int)

origin :: Pos
origin = (0,0)

left :: Pos -> Pos
left (x,y) = (x-1,y)

right :: Pos -> Pos
right (x,y) = (x+1,y)

--origin
--left origin
