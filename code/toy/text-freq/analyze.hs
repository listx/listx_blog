module Main where

import qualified Data.Text.Lazy.IO as T
import System.Environment

import TextFreq

main :: IO ()
main = do
	args <- getArgs
	src <- T.readFile $ args!!0
	dispFreqL $ freqL src
	putStrLn $ replicate 80 '-'
	dispFreqW $ freqW src
