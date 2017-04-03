module GCD where

import Foreign
import Foreign.C.Types

foreign import ccall "gcd"
  c_gcd :: CInt -> CInt -> CInt

f_gcd :: Int -> Int -> Int
f_gcd a b = fromIntegral $ c_gcd (fromIntegral a) (fromIntegral b)
