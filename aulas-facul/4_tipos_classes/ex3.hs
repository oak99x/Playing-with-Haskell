type Pos = (Int,Int)

origin :: Pos
origin = (0,0)

data Move = North | South | East | West deriving (Show)
--Move pertence a classe Show
--Move pode ser convertido em string para escrita no console
--deriving funciona com Eq, Ord, Enum, Bounded, Show, Read

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

--move North origin

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

--moves [South,East,East] origin
