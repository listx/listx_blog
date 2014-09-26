getParkingSpaces :: [Int] -> Either String Int
getParkingSpaces t
	| null t = Left "N/A"
	| otherwise = Right $ (maximum t) - (length t) + 1
