import Data.List ( intercalate )
import Data.List.Split ( splitOn )
import Debug.Trace ( trace )


main :: IO ()
main = interact $
  intercalate "," . map show . compile 0 . map read . splitOn ","


compile :: Int -> [Int] -> [Int]
compile index xs
  -- HALT
  | opcode == 99 = xs

  -- OPERATION
  | otherwise = compile (index + 4) (replace position total xs)
  where
    (opcode:_) = drop index xs
    (_:read1:read2:position:_) = drop index xs

    value1 = xs !! read1
    value2 = xs !! read2

    total = (operator opcode) value1 value2

    operator :: Int -> (Int -> Int -> Int)
    operator 1 = (+)
    operator 2 = (*)

    replace :: Int -> a -> [a] -> [a]
    replace index newVal xs =
      ys ++ [newVal] ++ zs
      where
        (ys, _:zs) = splitAt index xs
