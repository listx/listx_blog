import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Digest.Pure.SHA
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Word
import System.Random.MWC

--	...
--	gen <- initialize' $ year' + month' + day'
--	...

-- | Given a Word32 'num' generate a growing ByteString 'x' by repeatedly
-- generating a SHA1 digest, as a ByteString 'y', and appending it back to
-- 'x'. When 'x' is sufficiently large (at least 256 * 4 elements, as each
-- element is Word8 and we need 4 of these to get 1 Word32, and ultimately we
-- need 256 Word32s), convert it back down to [Word32] and call
-- System.Random.MWC's 'initialize' function on it.
initialize' :: PrimMonad m => Word32 -> m (Gen (PrimState m))
initialize' num = initialize . V.fromList . loop B.empty . B.pack $ octetsLE num
	where
	loop :: B.ByteString -> B.ByteString -> [Word32]
	loop acc bs
		| B.length acc >= (256 * 4) = take 256 $ toW32s acc
		| otherwise = loop (B.append acc $ sha1Bytes bs) (sha1Bytes bs)
	sha1Bytes :: B.ByteString -> B.ByteString
	sha1Bytes = bytestringDigest . sha1
	toW32s :: B.ByteString -> [Word32]
	toW32s = map fromOctetsLE . chop 4 . B.unpack
	chop :: Int -> [a] -> [[a]]
	chop _ [] = []
	chop n xs = take n xs : chop n (drop n xs)

-- For little-endian conversion.
octetsLE :: Word32 -> [Word8]
octetsLE w =
	[ fromIntegral w
	, fromIntegral (w `shiftR` 8)
	, fromIntegral (w `shiftR` 16)
	, fromIntegral (w `shiftR` 24)
	]

-- For big-endian conversion.
octetsBE :: Word32 -> [Word8]
octetsBE = reverse . octetsLE

fromOctetsBE :: [Word8] -> Word32
fromOctetsBE = foldl' accum 0
	where
	accum a o = (a `shiftL` 8) .|. fromIntegral o

fromOctetsLE :: [Word8] -> Word32
fromOctetsLE = fromOctetsBE . reverse
