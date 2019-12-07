import Debug.Trace
import Data.List.Split ( splitOn )
import Data.List ( intersect, sort )


type Canvas =
  [[Bool]]


data Instruction
  = GoUp Int
  | GoDown Int
  | GoLeft Int
  | GoRight Int
  deriving Show


type Location =
  (Int, Int)


sampleInput :: String
sampleInput =
  "R8,U5,L5,D3\nU7,R6,D4,L4"


-- >>> readInstruction "R8"
-- GoRight 8
readInstruction :: String -> Instruction
readInstruction (dir:dist)
  | dir == 'U' = GoUp    $ read dist
  | dir == 'D' = GoDown  $ read dist
  | dir == 'L' = GoLeft  $ read dist
  | dir == 'R' = GoRight $ read dist


-- >>> parseInput sampleInput
-- [ [GoRight 8,GoUp 5,GoLeft 5,GoDown 3]
-- , [GoUp 7,GoRight 6,GoDown 4,GoLeft 4]
-- ]
parseInput :: String -> [[Instruction]]
parseInput =
  map (map readInstruction . splitOn ",") . splitOn "\n"


-- >>> move (GoRight 8) (0, 0)
-- (8,0)
move :: Instruction -> Location -> Location
move (GoUp    dist) (xPos, yPos) = (xPos       , yPos + dist)
move (GoDown  dist) (xPos, yPos) = (xPos       , yPos - dist)
move (GoRight dist) (xPos, yPos) = (xPos + dist, yPos       )
move (GoLeft  dist) (xPos, yPos) = (xPos - dist, yPos       )


-- >>> pathfinder [(0, 0)] (head $ parseInput sampleInput)
-- [(3,2),(3,5),(8,5),(8,0),(0,0)]
pathfinder :: [Location] -> [Instruction] -> [Location]
pathfinder ls       []     = ls
pathfinder ls@(l:_) (i:is) = pathfinder (move i l : ls) is


-- >>> drawDot [[False, False], [False, False]] (1,0)
-- [[False,True],[False,False]]
drawDot :: Canvas -> Location -> Canvas
drawDot canvas (x, y)
  =  take y canvas
  ++ [take x (canvas !! y) ++ [True] ++ drop (x + 1) (canvas !! y)]
  ++ drop (y + 1) canvas


-- >>> drawLine [[False, False], [False, False]] (1,0) (0,0)
-- [ [True,True]
-- , [False,False]
-- ]
drawLine :: Canvas -> Location -> Location -> Canvas
drawLine canvas (x1, y1) (x2, y2)
  | x1 == x2 || y1 == y2 = foldl drawDot canvas dots
  where
    dots = [ (x, y)
           | x <- enumerateBetween x1 x2
           , y <- enumerateBetween y1 y2
           ]


-- >>> drawPath (initialiseCanvas 2 2) [(0,0), (1,0), (1, 1)]
-- [ [True,True]
-- , [False,True]
-- ]
drawPath :: Canvas -> [Location] -> Canvas
drawPath canvas (l:[]) = drawDot canvas l
drawPath canvas ls@(l1:l2:_) =
  drawPath (drawLine canvas l1 l2) (tail ls)


-- >>> enumerateBetween 10 1
-- [1,2,3,4,5,6,7,8,9,10]
enumerateBetween :: (Enum a, Ord a) => a -> a -> [a]
enumerateBetween x y
  | y > x     = [x..y]
  | otherwise = [y..x]


-- HACK: Initialise the canvas to a fixed size
-- >>> initialiseCanvas 2 3
-- [ [False,False]
-- , [False,False]
-- , [False,False]
-- ]
initialiseCanvas :: Int -> Int -> Canvas
initialiseCanvas x y =
  [ [ False | x <- [1..x] ] | y <- [1..y] ]


initialCanvas :: Canvas
initialCanvas =
  initialiseCanvas 100 100


intersectCanvas :: Canvas -> Canvas -> Canvas
intersectCanvas xss yss =
  [ [ x && y
    | i <- [0 .. length xs - 1]
    , let x = xs !! i
    , let y = ys !! i
    ]
  | j <- [0 .. length xss  - 1]
  , let xs = xss !! j
  , let ys = yss !! j
  ]


canvasToDots :: Canvas -> [Location]
canvasToDots xss =
  concat [ [ (i, j)  | (i, x) <- zip [0..] xs, x ]
         | (j, xs) <- zip [0..] xss
         ]


locationToDistance :: Location -> Int
locationToDistance (x, y) =
  abs x + abs y


main :: IO ()
main
  = interact
  $ show
  . (flip (!!)) 1
  . sort
  . map locationToDistance
  . canvasToDots
  . foldl1 intersectCanvas
  . map (drawPath initialCanvas . pathfinder [(0, 0)])
  . parseInput
