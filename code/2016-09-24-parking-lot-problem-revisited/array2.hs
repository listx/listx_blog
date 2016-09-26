import Data.Array (Array, elems)
import Data.Array.ST (runSTArray, newArray, writeArray)

checklist :: [Int] -> Array Int Bool
checklist xs = runSTArray $ do
	a <- newArray (0, n) False
	sequence [writeArray a x True | x <- xs, x < n]
	return a
	where
	n = length xs

search :: Array Int Bool -> Int
search =  length . takeWhile id . elems

minfreeArray2 :: [Int] -> Int
minfreeArray2 = search . checklist
