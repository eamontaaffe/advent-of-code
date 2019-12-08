import Data.List ( sort )
import Debug.Trace

-- >>> digitsToNumber [4, 5, 9, 0]
-- 4590
digitsToNumber :: [Int] -> Int
digitsToNumber =
  read . concat . map show


-- >>>  numberToDigits $ 4590
-- [4,5,9,0]
numberToDigits :: Int -> [Int]
numberToDigits =
  map (read . (:[])). show


-- >>> digitsNeverDecrease [9, 1, 2, 3, 3, 5, 5]
-- False
digitsNeverDecrease :: [Int] -> Bool
digitsNeverDecrease (_:[]) = True
digitsNeverDecrease xs@(x1:x2:_) =
  x1 <= x2 && digitsNeverDecrease (tail xs)


-- >>> atLeastNAdjacent 3 [1, 2, 3, 5, 4, 8, 8, 8]
-- True
atLeastNAdjacent :: Int -> [Int] -> Bool
atLeastNAdjacent n []       = False
atLeastNAdjacent n xs@(x:_) =
  length instances >= n || atLeastNAdjacent n (tail xs)
  where
    instances :: [Int]
    instances = filter (== x) xs


-- >>> exactlyNAdjacent 2 [1, 2, 3, 5, 4, 8, 8, 8, 1]
-- True
exactlyNAdjacent :: Int -> [Int] -> Bool
exactlyNAdjacent n xs =
  or $ map (\x -> (== n) . length . filter (== x) $ xs) xs


-- >>> length $ generatePasswordsPart1 367479 893698
-- 495
generatePasswordsPart1 :: Int -> Int -> [Int]
generatePasswordsPart1 lower upper =
  [ x

  -- IT HAS 6 DIGITS
  -- THE VALUE IS WITHIN SOME RANGE
  | x <- [lower .. upper]

  -- TWO ADJACENT DIGITS ARE THE SAME
  , atLeastNAdjacent 2 . numberToDigits $ x

  -- DIGITS NEVER DECREASE FROM LEFT TO RIGHT
  , digitsNeverDecrease . numberToDigits $ x
  ]


-- >>> length $ generatePasswordsPart2 367479 893698
-- 305
generatePasswordsPart2 :: Int -> Int -> [Int]
generatePasswordsPart2 lower upper =
  [ x

  -- IT HAS 6 DIGITS
  -- THE VALUE IS WITHIN SOME RANGE
  | x <- [lower .. upper]

  -- EXACTLY TWO ADJACENT DIGITS ARE THE SAME
  , exactlyNAdjacent 2 . numberToDigits $ x

  -- DIGITS NEVER DECREASE FROM LEFT TO RIGHT
  , digitsNeverDecrease . numberToDigits $ x
  ]
