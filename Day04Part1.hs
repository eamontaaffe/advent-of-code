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


-- >>> twoAdjacentDigitsAreTheSame [1, 2, 3, 5, 4, 8, 8]
-- True
twoAdjacentDigitsAreTheSame :: [Int] -> Bool
twoAdjacentDigitsAreTheSame (_:[])       = False
twoAdjacentDigitsAreTheSame xs@(x1:x2:_) =
  x1 == x2 || twoAdjacentDigitsAreTheSame (tail xs)


-- >>> length $ generatePasswords 367479 893698
-- 495
generatePasswords :: Int -> Int -> [Int]
generatePasswords lower upper =
  [ x

  -- IT HAS 6 DIGITS
  -- THE VALUE IS WITHIN SOME RANGE
  | x <- [lower .. upper]

  -- TWO ADJACENT DIGITS ARE THE SAME
  , twoAdjacentDigitsAreTheSame . numberToDigits $ x

  -- DIGITS NEVER DECREASE FROM LEFT TO RIGHT
  , digitsNeverDecrease . numberToDigits $ x
  ]
