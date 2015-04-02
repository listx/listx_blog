import Data.List
import Data.Maybe

getParkingSpace' :: [Int] -> Int
getParkingSpace' ts = fromMaybe
	(length ts)
	(elemIndex False . zipWith (==) [0..] $ sort ts)
