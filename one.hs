main :: IO ()
main = interact $
  show . sum . map (subtract 2 . floor . (/ 3)) . map read . words
