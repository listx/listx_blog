{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.Lazy.IO	as Lazy
import Clay
import Clay.Elements as E

main :: IO ()
main = Lazy.putStr $ renderWith compact [] myStylesheet

myStylesheet :: Css
myStylesheet = do
	html ? do
		margin (px 0) (px 0) (px 0) (px 0)
		padding (px 0) (px 0) (px 0) (px 0)
		fontSize (pt 14)
		backgroundColor (grayish 200)
		color (grayish 30)
		overflowY scroll
	body ? do
		width (px 700)
		margin (px 5) auto (px 20) auto
	"h2, h3" ? do
		fontWeight bold
	h1 ? do
		fontSize (pt 30)
		fontWeight normal
	E.div ? do
		"#header" & do
			textAlign $ alignSide sideCenter
		"#content" & do
			h1 ? do
				margin (px 2) 0 (px 10) 0
				borderBottom solid (px 4) (rgb 0 0 0)
			h2 ? do
				textDecoration underline
		"#footer" & do
			color (grayish 100)
			fontSize (pt 12)
			textAlign $ alignSide sideCenter
	code ? do
		fontSize (pt 10)
		backgroundColor $ rgb 230 230 250
		border solid (px 1) (rgb 170 170 200)
		borderRadius (px 3)
		padding 0 (px 4) 0 (px 4)
	pre ? do
		code ? do
			display block
			overflow auto
	a ? do
		":hover" & do
			textDecoration none
