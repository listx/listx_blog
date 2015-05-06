module Main where

import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Unmutate

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProps, unitTests]

data AP = AP [Integer]
	deriving (Eq, Show)

-- Generate a random arithmetic progression.
instance Arbitrary AP where
	arbitrary = do
		-- Choose random m (change between terms).
		m <- arbitrary :: Gen Int
		-- Choose random first term. It's important that we make it into an
		-- Integer type, because if we use Int we might end up with integer
		-- overflow if m is too large.
		t <- arbitrary :: Gen Integer
		-- Choose random length of 4 to 50.
		len <- choose (4, 50)
		return
			. AP
			. foldl (\acc n -> ((fromIntegral m)*n + t):acc) []
			$ take len [0..]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
	[ QC.testProperty "Unmutate (U(M(AP)) results in calculable (isJust))"
		(\(AP ts) -> isJust . unmutate $ mutate ts)
	, QC.testProperty "Unmutate (U(M(AP)) results in a nonempty list)"
		(\(AP ts) -> not
			. null
			. fromJust
			. unmutate
			$ mutate ts)
	, QC.testProperty "Unmutate (U(M(AP)) results in an arithmetic progression)"
		(\(AP ts) -> isArithmetic
			. fromJust
			. unmutate
			$ mutate ts)
	]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
	[ testCase "Unmutate (empty list is not calculable)"
		$ unmutate ([]::[Int])
		@?= Nothing
	, testCase "Unmutate (AP is all odd, so AP == MP)"
		$ unmutate [1, 3, 5, 7, 9 :: Int]
		@?= Just [1, 3, 5, 7, 9 :: Int]
	, testCase "Unmutate (m = 0, so all elements in mp are the same)"
		$ unmutate [5, 5, 5, 5, 5 :: Int]
		@?= Just [5, 5, 5, 5, 5 :: Int]
	, testCase "Unmutate (known case 1)"
		$ unmutate [1, 1, 3, 1, 5 :: Int]
		@?= Just [1, 2, 3, 4, 5 :: Int]
	, testCase "Unmutate (known case 2)"
		$ unmutate [7, 47, 5, 113, 73, 179, 53 :: Int]
		@?= Just [14, 47, 80, 113, 146, 179, 212 :: Int]
	, testCase "Unmutate (known case 3)"
		$ unmutate [749, 999, 125, 1 :: Int]
		@?= Just [1498, 999, 500, 1 :: Int]
	, testCase "Unmutate (known case 4)"
		$ unmutate [-11, 0, 11, 11, 33, 11 :: Int]
		@?= Just [-11, 0, 11, 22, 33, 44 :: Int]
	]
