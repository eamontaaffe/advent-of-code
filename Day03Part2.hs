import Data.List.Split ( splitOn )
import Data.List ( intersect, sort, nub )


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
readInstruction instruction@(dir:dist)
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


-- >>> pathFinder [(0, 0)] (head $ parseInput sampleInput)
-- [(3,2),(3,5),(8,5),(8,0),(0,0)]
pathFinder :: [Location] -> [Instruction] -> [Location]
pathFinder ls       []     = ls
pathFinder ls@(l:_) (i:is) = pathFinder (move i l : ls) is


-- >>> lineToDots (0, 0) (0, 3)
-- [(0,0),(0,1),(0,2),(0,3)]
lineToDots :: Location -> Location -> [Location]
lineToDots (x1, y1) (x2, y2) =
  [ (x, y) | x <- enumFromTo x1 x2, y <- enumFromTo y1 y2 ]
  where
    enumFromTo :: Int -> Int -> [Int]
    enumFromTo x y
      | y > x     = [x..y]
      | otherwise = [y..x]


-- >>> pathToDots [(0,0), (3,0)]
-- [(0,0),(1,0),(2,0),(3,0)]
pathToDots :: [Location] -> [Location]
pathToDots ls@(l1:[]) = ls
pathToDots ls@(l1:l2:_) =
  lineToDots l1 l2 ++ pathToDots (tail ls)


-- >>> locationToDistance (1, 3)
-- 4
locationToDistance :: Location -> Int
locationToDistance (x, y) =
  abs x + abs y


-- >>> pathCost [(3,2),(3,5),(8,5),(8,0),(0,0)] (3, 3)
-- True
-- 1
pathCost :: [Location] -> Location -> Int
pathCost ls@(l1:l2:_) l3
  | ifIntersect = distance l1 l3
  | otherwise = distance l1 l2 + pathCost (tail ls) l3
  where
    dots = lineToDots l1 l2
    ifIntersect = any ((==) l3) dots


-- >>> distance (0,0) (10,0)
-- 10
distance :: Location -> Location -> Int
distance (x1, y1) (x2, y2)
  | x1 == x2 || y1 == y2 = abs (x1 - x2) + abs (y1 - y2)


run :: String -> String
run input =
  show . minimum . map (foldl1 (+)) $ costs
  where
    instructionSets :: [[Instruction]]
    instructionSets = parseInput input

    paths :: [[Location]]
    paths = map (pathFinder [(0, 0)]) instructionSets

    dots :: [[Location]]
    dots = map pathToDots paths

    intersections :: [Location]
    intersections = filter ((/=) (0,0)) (foldl1 intersect dots)

    costs :: [[Int]]
    costs = [ [ pathCost (reverse path) intersection
              | path <- paths
              ]
            | intersection <- intersections
            ]


main :: IO ()
main = interact $ run
