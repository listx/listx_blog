module Main where

import Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import TextFreq

main :: IO ()
main = do
	fileList <- T.getContents
	src <- liftM T.concat . mapM (T.readFile . T.unpack) $ T.lines fileList
	dispFreqL $ freqL src
	putStrLn $ replicate 80 '-'
	dispFreqW $ freqW src
