{-# LANGUAGE RecordWildCards #-}

-- LICENSE: PUBLIC DOMAIN
--
-- Compile with `ghc --make -Wall -Werror -O2 -dynamic -o binary-search-hs
-- binary-search.hs'. For better conformance with the C and Ruby versions, we
-- use snake_case instead of camelCase wherever there is a direct parallel.
--
-- Interact with `ghci path/to/this/file`.
--
-- Usage: just execute the binary as-is without any arguments. To test the RNG,
-- call with the argument "rng".

module Main where

import Control.Monad
import Data.Bits
import Data.Word
import System.Environment
import System.Exit
import Text.Printf

u32_max :: Word32
u32_max = 0xffffffff

keys_total :: Int
keys_total = 1000000

data PCG32 = PCG32
  { state :: Word64
  , inc :: Word64
  }

pcg32_random_r :: PCG32 -> (Word32, PCG32)
pcg32_random_r rng@PCG32{..} = (result, rng {state = state'})
  where
  state' :: Word64
  state' = state * 6364136223846793005 + (inc .|. 1)
  xorshifted :: Word32
  xorshifted = fromIntegral $ shiftR (xor (shiftR state 18) state) 27
  rot :: Word32
  rot = fromIntegral $ shiftR state 59
  result :: Word32
  result = fromIntegral
    $ (shiftR xorshifted $ fromIntegral rot)
      .|. (shiftL xorshifted $ fromIntegral ((-rot) .&. 31))

uniform32 :: Word32 -> PCG32 -> (Word32, PCG32)
uniform32 range rng = find_within_range rng
  where
  rand_excess :: Word32
  rand_excess = mod ((mod u32_max range) + 1) range
  rand_limit :: Word32
  rand_limit = u32_max - rand_excess
  find_within_range rng' = if x > rand_limit
    then find_within_range rng''
    else (mod x range, rng'')
    where
    (x, rng'') = pcg32_random_r rng'

init_array :: Int -> Bool -> PCG32 -> ([Word32], Word32, PCG32)
init_array keys_size has_key rng0 = (keys, key, rng3)
  where
  (keys', rng1) = genKeysList [] 0 0 rng0
  -- Need to reverse the list, because Haskell (like all Lispy languages?)
  -- builds a list backwards when using the cons (:) operator.
  keys = reverse keys'
  genKeysList :: [Word32] -> Int -> Int -> PCG32 -> ([Word32], PCG32)
  genKeysList arr i j0 rng = if i < keys_size
    then genKeysList ((i' + j2'):arr) (i + 1) j2 rng'
    else (arr, rng)
    where
    i' = fromIntegral i
    j2' = fromIntegral j2
    (j1, rng') = uniform32 2 rng
    j2 = j0 + fromIntegral j1
  (key, rng3) = if has_key
    then
      let
        (idx, rng2) = uniform32 (fromIntegral keys_total) rng1
      in
      (keys!!(fromIntegral idx), rng2)
    else (keys!!(keys_total - 1) + 1, rng1)

-- We use min' and max' because the non-apostrophe versions name-clash with
-- Prelude's own functions. We could hide Prelude's imports, but that seems too
-- roundabout.
binary_search :: [Word32] -> Word32 -> Int -> Int -> Maybe Int
binary_search keys key min' max'
  | list_size == 0 = Nothing
  | key < keys!!mid = binary_search keys key min' (mid - 1)
  | key > keys!!mid = binary_search keys key (mid + 1) max'
  | otherwise = Just mid
  where
  list_size = (max' - min') + 1
  mid = (div list_size 2) + min'

main :: IO ()
main = do
  let
    rng0 = PCG32
      { state = 0x1234567890abcdef
      , inc = 0x1234567890abcdef
      }
  args <- getArgs
  when (args == ["rng"]) $ do
    putStrLn "Running RNG self-test"
    let
      (num0, rng1) = pcg32_random_r rng0
    putStrLn $ show num0
    rng2 <- foldM warmupRng rng1 [0..999999::Int]
    let
      (num1, rng3) = pcg32_random_r rng2
    putStrLn $ show num1
    rng4 <- foldM testUniform32 rng3 [0..99::Int]
    _ <- foldM testArray rng4 [0..9::Int]
    putStrLn "Done."
    putStrLn "END HASKELL VERSION"
    exitSuccess
  _ <- foldM testBinarySearch rng0 [0..19::Int]
  putStrLn "END HASKELL VERSION"
  where
  warmupRng rng _ = return . snd $ pcg32_random_r rng
  testUniform32 rng _ = do
    putStrLn $ show num
    return rng'
    where
    (num, rng') = uniform32 (div u32_max 2 + div u32_max 3) rng
  testArray rng0 i = do
    putStrLn $ "last number in array "
      ++ show i
      ++ " for key "
      ++ show key
      ++ ": "
      ++ show (keys!!(keys_total - 1))
    return rng2
    where
    (res, rng1) = uniform32 2 rng0
    has_key = res == 1
    (keys, key, rng2) = init_array keys_total has_key rng1
  testBinarySearch rng0 i = do
    printf "%02d - " (i + 1)
    case foundMid of
      Just mid -> putStrLn $ "key `"
        ++ show key
        ++ "' found at keys["
        ++ show mid
        ++ "]."
      Nothing -> putStrLn $ "key `"
        ++ show key
        ++ "' not found."
    return rng2
    where
    (res, rng1) = uniform32 2 rng0
    has_key = res == 1
    (keys, key, rng2) = init_array keys_total has_key rng1
    min' = 0
    max' = keys_total - 1
    foundMid = binary_search keys key min' max'
