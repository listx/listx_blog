import Data.List ((\\))

minfreeNaive :: [Int] -> Int
minfreeNaive xs = head ([0..] \\ xs)
