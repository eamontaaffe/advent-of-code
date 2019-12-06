import Data.List.Split ( splitOn )


main :: IO ()
main = interact $
  show . compile . map read . splitOn ","


replace :: Int -> a -> [a] -> [a]
replace index newVal xs =
  ys ++ [newVal] ++ zs
  where
    (ys, _:zs) = splitAt index xs


compile :: [Int] -> [Int]

-- HALT
compile (99:_) = [99]

-- ADD
compile xs@(1:read1:read2:position:_) =
  take 4 ys ++ compile (drop 4 ys)
  where
    value1 = xs !! read1
    value2 = xs !! read2
    total = value1 + value2
    ys = replace position total xs

-- -- MULTIPLY
compile xs@(2:read1:read2:position:_) =
  take 4 ys ++ compile (drop 4 ys)
  where
    value1 = xs !! read1
    value2 = xs !! read2
    total = value1 * value2
    ys = replace position total xs
