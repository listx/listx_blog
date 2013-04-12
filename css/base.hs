{-# LANGUAGE OverloadedStrings #-}

import Data.Bits
import qualified Data.Text.Lazy.IO as T
import Clay

main :: IO ()
main = T.putStr $ renderWith compact [] myStylesheet

div' = Clay.div

rgbHex :: Int -> Color
rgbHex rgb'
	| rgb' > 0xffffff || rgb' < 0 = error "invalid hex range"
	| otherwise = rgb r g b
	where
	r = fromIntegral $ (shiftR rgb' 16) .&. 0xFF
	g = fromIntegral $ (shiftR rgb' 8) .&. 0xFF
	b = fromIntegral $ rgb' .&. 0xFF

myStylesheet :: Css
myStylesheet = do
	html ? do
		ev margin 0
		ev padding 0
		fontSize (pt 14)
		backgroundColor (rgb 200 200 210)
		color (grayish 30)
		overflowY scroll
	body ? do
		width (px 750)
		hv margin 0 auto
		sup ? do
			"vertical-align" -: "top"
			fontSize (em 0.6)
	"h2, h3" ? do
		fontWeight bold
	h1 ? do
		fontSize (pt 30)
		fontWeight normal
	div' ? do
		"#header" & do
			hv margin (em 0.5) 0
			textAlign $ alignSide sideCenter
		"#content" & do
			hv padding (em 1) (em 2)
			ev borderRadius (px 3)
			backgroundColor (rgbHex 0xeeeeee)
			boxShadow (px 0) (px 0) (px 3) (rgbHex 0x666666)
			h1 ? do
				margin (px 2) 0 (px 10) 0
				borderBottom solid (px 2) (rgb 0 0 0)
			h2 ? do
				textDecoration underline
		"#footer" & do
			margin (em 0.5) 0 (em 0.8) 0
			color (grayish 100)
			fontSize (pt 12)
			textAlign $ alignSide sideCenter
	code ? do
		fontSize (pt 10)
		color (grayish 51)
		backgroundColor $ grayish 248
		border solid (px 1) (grayish 204)
		ev borderRadius (px 3)
		hv padding 0 (px 4)
	pre ? do
		code ? do
			display block
			overflow auto
			hv padding (px 6) (px 10)
	a ? do
		":hover" & do
			textDecoration none

-- | A horizontal/vertical size helper. It accepts a function and two sizes for
-- the horizontal and vertical parts. E.g., instead of calling
-- 		padding (px 6) (px 10) (px 6) (px 10)
-- you can simply do
--		hv padding (px 6) (px 10)
-- to save some keystrokes and decrease the chance of typos.
hv :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
hv f a b = f a b a b

-- | Like "hv", but uses the same size for *everything*.
ev :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
ev f a = f a a a a
