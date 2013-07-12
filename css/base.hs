{-# LANGUAGE OverloadedStrings #-}

import Data.Bits
import qualified Data.Text.Lazy.IO as T
import Clay

main :: IO ()
main = T.putStr $ renderWith compact myStylesheet

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
		backgroundColor (rgbHex bgHex)
		color (grayish 30)
		overflowY scroll
		body ? do
			width (px cPageWidth)
			vh margin 0 auto
			a ? do
				":hover" & do
					textDecoration none
					textShadow (px 0) (px 0) (px 2) (rgbHex $ shadowHex - 0x222222)
			sup ? do
				"vertical-align" -: "top"
				fontSize (em 0.6)
			div' ? do
				"#header" & do
					vh margin (em 0.5) 0
					textAlign $ alignSide sideCenter
				"#content" & do
					vh padding (em 1) (em 2)
					ev borderRadius (px 3)
					backgroundColor (rgbHex 0xeeeeee)
					boxShadow (px 0) (px 0) (px 3) (rgbHex 0x666666)
					h1 ? do
						fontSize (pt 30)
						fontWeight normal
						margin (px 2) 0 (px 10) 0
						borderBottom solid (px 2) (rgb 0 0 0)
					h2 ? do
						margin 0 0 0 (em (-0.4))
						fontWeight bold
						textDecoration underline
					h3 ? do
						margin 0 0 0 (em (-0.4))
						fontWeight bold
					ul ? do
						"#flushLeft" & do
							margin 0 0 0 (px (-20))
					p ? do
						"#taglist" & do
							fontSize (pt 12)
					div' ? do
						".figure" & do
							-- center images
							"display" -: "table"
							vh margin 0 auto
					code ? do -- single-line `code`
						fontSize (pt 10)
						color (grayish 51)
						backgroundColor $ grayish 248
						border solid (px 1) (grayish 204)
						ev borderRadius (px 3)
						vh padding 0 (px 4)
					table ? do -- code with line numbers
						display block
						overflow auto
						".sourceCode" & do
							backgroundColor $ grayish 248
							border solid (px 1) (grayish 204)
							ev borderRadius (px 3)
							boxShadow (px 0) (px 0) (px 3) (rgbHex shadowHex)
							tr ? do
								td ? do
									".lineNumbers" & do
										vh padding 0 (px 4)
										pre ? do
											ev margin 0
											textAlign $ alignSide sideRight
											fontSize (pt 10)
											color (rgbHex $ bgHex - 0x151515)
								td ? do
									".sourceCode" & do
										ev padding 0
										pre ? do
											ev margin 0
											code ? do
												display block
												overflow auto
												vh padding (px 4) (px 2)
												"border-style" -: "none"
						ev borderRadius (px 3)
					pre ? do -- <pre><code> is generated if there is multiline (``` ... ```) code
						code ? do
							display block
							overflow auto
							vh padding (px 6) (px 10)
				"#footer" & do
					margin (em 0.5) 0 (em 0.8) 0
					color (grayish 100)
					fontSize (pt 12)
					textAlign $ alignSide sideCenter
	where
	div' = Clay.div
	cPageWidth :: Integer
	cPageWidth = 750
	bgHex :: Int
	bgHex = 0xc8c8d2
	shadowHex :: Int
	shadowHex = 0xdcdcd0

-- | A horizontal/vertical size helper. It accepts a function and two sizes for
-- the horizontal and vertical parts. E.g., instead of calling
-- 		padding (px 6) (px 10) (px 6) (px 10)
-- you can simply do
--		vh padding (px 6) (px 10)
-- to save some keystrokes and decrease the chance of typos.
vh :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
vh f a b = f a b a b

-- | Like "vh", but uses the same size for *everything*.
ev :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
ev f a = f a a a a
