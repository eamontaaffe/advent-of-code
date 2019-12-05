main :: IO ()
main = interact $
  show . sum . map fuelRequirement . map read . words


fuelRequirement :: Integer -> Integer
fuelRequirement mass
  | calcFuel > 0 = calcFuel + (fuelRequirement calcFuel)
  | otherwise = 0
  where
    calcFuel :: Integer
    calcFuel = subtract 2 . (`div` 3) $ mass
