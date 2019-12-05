main :: IO ()
main = interact $
  show . sum . map (subtract 2 . (`div` 3)) . map read . words
