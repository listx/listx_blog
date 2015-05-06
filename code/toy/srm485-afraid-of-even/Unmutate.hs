module Unmutate where

-- Convert mutated numbers back into an arithmetic progression (if possible).
unmutate :: Integral a => [a] -> Maybe [a]
unmutate ts
	-- The input must be at least 4 terms!
	| length ts < 4 = Nothing
	| all (==t) ts = Just ts
	| isArithmetic ts = Just ts
	| mutate bp1 == ts = Just bp1
	| mutate bp2 == ts = Just bp2
	-- Could optionally raise an error saying ts was not derived from an
	-- arithmetic progression in the first place!
	| otherwise = Nothing
	where
	t = head ts
	-- O_F, O, O_F, O...
	bp1Terms = everyNth0 2 ts
	bp1 = makeBP (length ts) False bp1Terms
	-- O, O_F, O, O_F...
	bp2Terms = everyNth 2 ts
	bp2 = makeBP (length ts) True bp2Terms

-- Create a tentative arithmetic progression "BP" from the given arguments. The
-- `makeFirstTerm` boolean determines whether we are dealing with a "O_F, O,
-- O_F, O..." or a "O, O_F, O, O_F" pattern.
makeBP :: Integral a => Int -> Bool -> [a] -> [a]
makeBP len makeFirstTerm originals
	| length originals < 2 = []
	| makeFirstTerm = (o0 - m) : init bp
	| otherwise = bp
	where
	o0 = originals!!0
	o1 = originals!!1
	m = div (o1 - o0) 2
	bp = reverse
		. foldl (\acc n -> (m*n + o0):acc) []
		$ take len [0..]

isArithmetic :: Integral a => [a] -> Bool
isArithmetic ts
	-- A progression (before we even get to whether it is arithmetic) must have
	-- at least 2 terms in it.
	| length ts < 2 = False
	| otherwise = all (==m) $ zipWith (-) (tail ts) (init ts)
	where
	t0 = ts!!0
	t1 = ts!!1
	m = t1 - t0

mutate :: Integral a => [a] -> [a]
mutate = map makeOdd
	where
	makeOdd :: Integral a => a -> a
	makeOdd n
		-- 0 cannot be turned into an odd number! Keep it as is.
		| n == 0 = 0
		| odd n = n
		| otherwise = makeOdd $ div n 2

-- Given a list of 'a's, return the elements at indices [0n, 1n, 2n, 3n, ...].
-- E.g., given a list [0..] and n = 2, we get [0, 2, 4, 6..]. From
-- http://stackoverflow.com/a/2028758/437583.
everyNth0 :: Int -> [a] -> [a]
everyNth0 _ [] = []
everyNth0 n as = head as : everyNth0 n (drop n as)

-- Same as `everyNth0`, but start at the nth element. E.g., given a list [0..]
-- and n = 2, we get [1, 3, 5, 7..].
everyNth :: Int -> [a] -> [a]
everyNth n = everyNth0 n . drop (n - 1)
