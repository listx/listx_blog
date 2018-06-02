import Data.List (partition)

minfreeRecurse :: [Int] -> Int
minfreeRecurse xs = minfrom 0 (length xs, xs)

minfrom :: Int -> (Int, [Int]) -> Int
minfrom a (n, xs)
  | (n == 0) = a
  | (m == b - a) = minfrom b (n - m, bs)
  | otherwise = minfrom a (m, as)
  where
  (as, bs) = partition (<b) xs
  b = a + (div n 2) + 1
  m = length as
