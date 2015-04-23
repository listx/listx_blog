module TextFreq where

import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import qualified Data.Text.Lazy as T
import Data.Word
import qualified Text.Printf as TP

type LHash = M.Map Char Word64

type WProto = T.Text
type WHash = M.Map WProto Word64
data WFSM
	= WordIn
	| WordOutMaybe
	| WordOut
	deriving (Eq)
data WBuild = WBuild WFSM WProto WHash

freqL :: T.Text -> LHash
freqL = T.foldl step occs
	where
	occs = M.empty
	step lhash c
		| isAlpha c = M.insertWith (+) (toLower c) 1 lhash
		| otherwise = lhash

freqW :: T.Text -> WHash
freqW = (\(WBuild _ _ whash) -> whash) . T.foldl step occs
	where
	-- Use WordOut as the initial state for WFSM, because we're starting from
	-- nothing!
	occs :: WBuild
	occs = WBuild WordOut T.empty M.empty
	step wb@(WBuild wfsm wproto whash) c
		-- Letter.
		| isAlpha c = case wfsm of
			-- This is when we first encounter a letter.
			WordOut -> WBuild WordIn (T.singleton c') whash
			_ -> WBuild WordIn (T.snoc wproto c') whash
		-- Apostrophe. We ignore all leading apostrophes and only store
		-- apostrophes at the end of a word, such as "goin'".
		| c == '\'' = case wfsm of
			-- This is when we encounter an apostrophe either at the middle or
			-- end of a word.
			WordIn -> WBuild WordOutMaybe (T.snoc wproto c') whash
			-- E.g., "goin''" (a contracted "goin''" ending with a nested inner
			-- quote). We store it as "goin'".
			WordOutMaybe -> WBuild WordOut T.empty
				$ M.insertWith (+) wproto 1 whash
			-- Already out of a word area, such as a space character. We do
			-- nothing.
			WordOut -> wb
		-- If we're looking at neither a letter nor an apostrophe.
		| otherwise = case wfsm of
			-- A series of nonsense chars; ignore.
			WordOut -> wb
			-- End of a word.
			_ -> WBuild WordOut T.empty
				$ M.insertWith (+) wproto 1 whash
		where
		c' = toLower c

dispFreqL :: LHash -> IO ()
dispFreqL lhash = mapM_ f . reverse . sortBy (comparing snd) $ M.toList lhash
	where
	total :: Word64
	total = sum $ M.elems lhash
	f :: (Char, Word64) -> IO ()
	f (c, n) = putStrLn $ msg1 ++ msg2 ++ msg3
		where
		perc :: Double
		perc
			| total == 0 = 0
			| otherwise = (fromIntegral n) / (fromIntegral total) * 100
		msg1 = [c] ++ " = "
		msg2 = TP.printf "%.2f%%" perc
		msg3 = " (" ++ show n ++ " occurrences)"

dispFreqW :: WHash -> IO ()
dispFreqW whash = mapM_ f . take 100 . reverse . sortBy (comparing snd) $ M.toList whash
	where
	total :: Word64
	total = sum $ M.elems whash
	f :: (WProto, Word64) -> IO ()
	f (w, n) = putStrLn $ msg1 ++ msg2 ++ msg3
		where
		perc :: Double
		perc
			| total == 0 = 0
			| otherwise = (fromIntegral n) / (fromIntegral total) * 100
		msg1 = T.unpack w ++ " = "
		msg2 = TP.printf "%.2f%%" perc
		msg3 = " (" ++ show n ++ " occurrences)"
