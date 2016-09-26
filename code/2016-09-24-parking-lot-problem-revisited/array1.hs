import Data.Array (Array, accumArray, elems)

minfreeArray1 :: [Int] -> Int
minfreeArray1 = search . checklist

-- Look for first False value (empty space).
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- Convert a list of parked spaces into an ordered list of Boolean values in the
-- range *reducedXs*.
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray
	(||) -- accumulating function
	False -- initial value
	(0, n) -- bounds of the array
	(zip (filter (<n) xs) (repeat True)) -- association list of `(index, value)' pairs
	where
	n = length xs
