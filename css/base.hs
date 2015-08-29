{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Bits
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Clay
import qualified Clay.Display as CD
import Clay.Font
import qualified Clay.Text as CT

main :: IO ()
main = T.putStr $ renderWith compact [] myStylesheet

rgbHex :: Int -> Color
rgbHex rgb'
	| rgb' > 0xffffff || rgb' < 0 = error "invalid hex range"
	| otherwise = rgb rr gg bb
	where
	rr = fromIntegral $ (shiftR rgb' 16) .&. 0xFF
	gg = fromIntegral $ (shiftR rgb' 8) .&. 0xFF
	bb = fromIntegral $ rgb' .&. 0xFF

myStylesheet :: Css
myStylesheet = do
	html ? do
		noMargin
		ev padding $ px 0
		let
			bgHexColor = 0xd1dbbd
		backgroundColor $ rgbHex bgHexColor
		color (grayish 30)
		overflowY scroll
		body ? do
			width (px cPageWidth)
			vh margin (px 0) auto
			a ? do
				let
					hoverLink = do
						textDecoration none
						hover & do
							textShadow (px 0) (px 0) (px 2)
								(rgbHex $ shadowHex - 0x222222)
				link & do
					color (rgbHex 0x0077ff)
				visited & do
					color (rgbHex 0x007020)
				hover & do
					textDecoration none
				".history" & hoverLink
				".raw" & hoverLink
			sup ? do
				"vertical-align" -: "top"
				fontSizeCustom Clay.Font.small
			hr ? do
				noMargin
				height (px 1)
			div' ? do
				".center" & do
					textAlign $ alignSide sideCenter
			div' ? do
				"#header" & do
					vh padding (em 0.5) 0
					textAlign $ alignSide sideCenter
				"#content" & do
					ev borderRadius (px 6)
					backgroundColor $ rgbHex 0xffffff
					boxShadow (px 0) (px 0) (px 3) (rgbHex 0x666666)
					paddingBottom (em 2)
					h1 ? do
						noMargin
						headerIndent
						paragraphIndentRight
						paddingBottom (em 0.5)
						fontWeight normal
						addHeadingSymbol
						-- For the title of the page, center-align it.
						".center" & do
							noMargin
							paddingTop (em 1)
							paddingBottom (em 0.5)
							fontSize (pt 30)
							fontWeight bold
							headerIndentRight
							textAlign $ alignSide sideCenter
							textDecoration none
							before & do
								CT.content (none :: Content)
						code ? do
							vh margin (px 0) (px 4)
					-- For raw tables (e.g., org-mode's tables.)
					table ? do
						-- horizontally center it
						headerIndent
						headerIndentRight
					div' ? do
						".info" & do
							paddingBottom (em 1)
						-- This is when we use our custom 'import source code'
						-- syntax with '- i <filename>'.
						".code-and-raw" & do
							marginBottom (em 1)
							boxBorders
							table ? do
								".sourceCode" & do
									-- if a table is part of "code-and-raw",
									-- reduce bottom margin to 0
									marginBottom $ px 0
									paddingTop (em 0.5)
									paddingBottom (em 0.5)
									"border-style" -: "none"
									display block
									overflow auto
									backgroundColor codeBg
								sourceCodeMarkdownNumberlines False
						".raw-link" & do
							backgroundColor $ rgbHex (codeBgHex - 0x080808)
							fontSizeCustom Clay.Font.small
							textAlign $ alignSide sideCenter
					h2 ? do
						noMargin
						headerIndent
						paragraphIndentRight
						paddingBottom (em 0.5)
						fontWeight normal
						code ? do
							vh margin (px 0) (px 4)
						addHeadingSymbol
					h3 ? do
						noMargin
						headerIndent
						paragraphIndentRight
						paddingBottom (em 0.5)
						fontWeight normal
						fontStyle italic
						code ? do
							vh margin (px 0) (px 4)
						addHeadingSymbol
					h4 ? do
						noMargin
						headerIndent
						paragraphIndentRight
						paddingBottom (em 0.5)
						code ? do
							vh margin (px 0) (px 4)
						addHeadingSymbol
					h5 ? do
						noMargin
						headerIndent
						paragraphIndentRight
						paddingBottom (em 0.5)
						code ? do
							vh margin (px 0) (px 4)
						addHeadingSymbol
					ol ? do
						noMargin
						paragraphListIndent
						paddingBottom (em 1)
						p ? do
							ev padding $ px 0
					ul ? do
						noMargin
						paragraphListIndent
						paddingBottom (em 1)
						ul ? do
							noMargin
							headerIndent
							paddingBottom $ px 0
					p ? do
						noMargin
						paddingBottom (em 1)
						paragraphIndent
					-- single-line `code`
					code ? do
						color (grayish 51)
						backgroundColor codeBg
						border solid (px 1) (grayish 204)
						ev borderRadius (px 3)
						vh padding 0 (px 4)
					div' ? do
						".figure" & do
							-- center images
							display CD.table
							vh margin (px 0)auto
						".footnotes" & do
							backgroundColor footnotesBg
							boxBorders
							hr ? do
								display displayNone
							paddingBottom $ px 0
							ol ? do
								paragraphIndent
								paddingTop (em 1)
								paddingBottom (em 0)
								li ? do
									paddingLeft $ px 0
									p ? do
										paddingBottom (em 1)
					-- <pre><code> is generated if there is multiline (``` ... ```) code
					pre ? do
						noMargin
						vh padding 0 $ px 0
						code ? do
							"-moz-tab-size" -: "4"
							"-o-tab-size" -: "4"
							"tab-size" -: "4"
							display block
							overflow auto
							-- we indent a little bit more here compared to
							-- table.sourceCode, because here we have to
							-- compensate for the fact that we don't have line
							-- numbers to push our code a little bit further
							-- right
							paragraphIndent'
							color (grayish 51)
							backgroundColor codeBg
							ev borderRadius (px 0)
							"border-style" -: "none"
							boxBorders
							marginBottom (em 1)
							paddingTop (em 1)
							paddingBottom (em 1)
					table ? do -- code with line numbers
						"-moz-tab-size" -: "4"
						"-o-tab-size" -: "4"
						"tab-size" -: "4"
						paddingRight $ px 0
						marginBottom (em 1)
						-- posts archive
						".ul" & do
							paragraphIndent
							marginBottom $ px 0
							paddingBottom (em 1)
							tr ? do
								td ? do
									".date" & do
										"vertical-align" -: "text-top"
										whiteSpace nowrap
						".gallery" & do
							headerIndent
							headerIndentRight
							marginBottom $ em 0
							fontSizeCustom Clay.Font.small
							tr ? do
								textAlign $ alignSide sideCenter
								"#header" & do
									fontSizeCustom Clay.Font.medium
									fontWeight bold
									textDecoration underline
						sourceCodeMarkdownNumberlines True
				"#footer" & do
					-- Margins of adjacent elements are *overlap*, unlike
					-- padding. We use a 1em top-side margin here, because,
					-- e.g., code listings sometimes have a bottom margin, and
					-- if the blog post ends with a code listing, we don't want
					-- to render both the listing's bottom spacing and the top
					-- spacing of the footer.
					marginTop (em 1)
					paddingBottom (em 1)
					color (grayish 100)
					fontSizeCustom Clay.Font.medium
					textAlign $ alignSide sideCenter
	where
	div' = Clay.div
	cPageWidth :: Integer
	cPageWidth = 900
	bgHex :: Int
	bgHex = 0xc8c8d2
	shadowHex :: Int
	shadowHex = 0xdcdcd0
	codeBgHex :: Int
	codeBgHex = 0xfdf6e3
	footnotesBgHex :: Int
	footnotesBgHex = 0xe9f7ff
	codeBg :: Color
	codeBg = rgbHex codeBgHex
	footnotesBg :: Color
	footnotesBg = rgbHex footnotesBgHex
	headerIndent = "padding-left" -: "12%"
	headerIndentRight = "padding-right" -: "12%"
	paragraphIndent = do
		"padding-left" -: "12%"
		paragraphIndentRight
	paragraphIndentRight = do
		"padding-right" -: "12%"
	paragraphIndent' = do
		"padding-left" -: "15%"
		paragraphIndentRight
	paragraphListIndent = do
		"padding-left" -: "18%"
		paragraphIndentRight
	sourceCodeMarkdownNumberlines addBorder = ".sourceCode" & do
		paragraphIndent
		paddingTop (em 0.5)
		paddingBottom (em 0.5)
		display block
		overflow auto
		backgroundColor codeBg
		when addBorder $ do
			boxBorders
		tr ? do
			td ? do
				".lineNumbers" & do
					pre ? do
						noMargin
						textAlign $ alignSide sideRight
						color (rgbHex $ bgHex - 0x151515)
				".sourceCode" & do
					paddingLeft $ px 0
					pre ? do
						ev margin $ px 0
						code ? do
							ev margin $ px 0
							padding 0 0 0 (em 1)
							"border-style" -: "none"
							boxShadow (px 0) (px 0) (px 0) (rgbHex shadowHex)
	addHeadingSymbol = before & do
		CT.content $ stringContent symbol
		position relative
		bottom (em 0.1)
		where
		symbol = T.toStrict
			. T.pack
			$ (toEnum 0x25a0) -- box character
			: (toEnum 0x00a0) -- nonbreaking space character
			: (toEnum 0x00a0) -- nonbreaking space character
			: []
	boxBorders = do
		borderTop solid (px 1) (grayish 204)
		borderBottom solid (px 1) (grayish 204)
		boxShadow (px 0) (px 0) (px 3) (rgbHex shadowHex)
-- | A horizontal/vertical size helper. It accepts a function and two sizes for
-- the horizontal and vertical parts. E.g., instead of calling
-- 		padding (px 6) (px 10) (px 6) (px 10)
-- you can simply do
--		vh padding (px 6) (px 10)
-- to save some keystrokes and decrease the chance of typos.
vh :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
vh f x y = f x y x y

-- | Like "vh", but uses the same size for *everything*.
ev :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
ev f x = f x x x x

noMargin :: Css
noMargin = margin 0 0 0 $ px 0
