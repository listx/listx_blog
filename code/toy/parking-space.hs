import Data.List

getParkingSpace :: [Int] -> Int
getParkingSpace ts
  | null ts = 0
  | otherwise = loop 0 $ sort ts
  where
  loop i xs
    | i == length xs = i
    | i < xs !! i = i
    | otherwise = loop (i + 1) xs
