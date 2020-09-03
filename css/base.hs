{-# LANGUAGE OverloadedStrings #-}

import Data.Bits
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Clay hiding (vh)
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
    backgroundColor $ rgbHex bgHex
    color textColor
    overflowY scroll
    body ? do
      width (px cPageWidth)
      vh margin (px 0) auto
      a ? do
        outline none (px 1) (rgbHex 0x000000)
        fontWeight bold
        textDecoration none
        link & do
          color (rgbHex linkHex)
        visited & do
          color (rgbHex linkVisitedHex)
        hover & do
          textDecoration underline
          ".section-heading" & do
            textDecoration underline
        ".section-heading" & do
          color textColor
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
          backgroundColor $ rgbHex bgHex
          paddingBottom (em 2)
          h1 <> h2 <> h3 <> h4 <> h5 ? do
            noMargin
            headerIndent
            paragraphIndentRight
            paddingBottom (em 0.5)
            fontWeight normal
            code ? do
              vh margin (px 0) (px 4)
          h1 ? do
            addHeadingSymbol uDoubleSquare
            -- For the title of the page, center-align it.
            ".center" & do
              noMargin
              paddingTop (em 0.2)
              paddingBottom (em 0.5)
              fontSize (pt 30)
              fontWeight bold
              headerIndentRight
              textAlign $ alignSide sideCenter
              textDecoration none
              before & do
                CT.content (none :: Content)
          h2 ? do
            addHeadingSymbol uBlackSquare
          h3 ? do
            addHeadingSymbol uWhiteSquare
          h4 ? do
            addHeadingSymbol uBlackDiamond
          h5 ? do
            addHeadingSymbol uDoubleDiamond
          h6 ? do
            addHeadingSymbol uWhiteDiamond
          -- For raw tables (e.g., org-mode's tables.)
          table ? do
            -- horizontally center it
            vh margin (px 0) auto
            marginBottom (em 1)
            borderCollapse collapse
            td ? do
              vh padding (em 0) (em 0.5)
            td |+ td ? do
              borderLeft solid (px 1) (rgbHex 0x000000)
          table ? do
            ".ul" & do
              borderCollapse separate
              td ? do
                "border-style" -: "none"
          div' ? do
            ".info" & do
              paddingBottom (em 1)
            -- This is when we use our custom 'import source code'
            -- syntax with '- i <filename>'.
            ".code-and-raw" & do
              hSymmetricGradient (rgbHex bgHex) codeBg 80
              marginBottom (em 1)
              table ? do
                ".sourceCode" & do
                  -- if a table is part of "code-and-raw",
                  -- reduce bottom margin to 0
                  marginBottom $ px 0
                  paddingBottom (em 0.5)
                  "border-style" -: "none"
                  tbody ? do
                    tr ? do
                      td ? do
                        pre ? do
                          code ? do
                            backgroundImage none
                            fontWeight normal
            ".raw-link" & do
              table ? do
                ".noPaddingBottom" & do
                  paddingBottom (em 0)
              color $ rgbHex lineNumHex
              code ? do
                vh padding (px 0) (px 0)
                paddingLeft $ em 1
              a ? do
                ".raw" & do
                  marginLeft $ em 0
                visited & do
                  color (rgbHex linkHex)
                fontWeight bold
            ".lineCntMax100" & do
              marginLeft (em (-0.58))
            ".lineCntMax1000" & do
              marginLeft (em (-1.16))
            ".lineCntMax10000" & do
              marginLeft (em (-2.32))
            ".sourceCode" & do
              paragraphIndent
              pre ? do
                code ? do
                  "padding-left" -: "0"
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
            textAlign justify
            "#taglist" & do
              paddingTop $ em 2
              paddingBottom $ px 0
          -- single-line `code`
          code ? do
            vh padding 0 (em 0.10)
            fontWeight bold
          div' ? do
            ".figure" & do
              -- center images
              display CD.displayTable
              vh margin (px 0) auto
            ".footnotes" & do
              hSymmetricGradient (rgbHex bgHex) footnotesBg 80
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
          blockquote ? do
            noMargin
            vh padding 0 $ px 0
            "-moz-tab-size" -: "4"
            "-o-tab-size" -: "4"
            "tab-size" -: "4"
            -- we indent a little bit more here compared to
            -- table.sourceCode, because here we have to
            -- compensate for the fact that we don't have line
            -- numbers to push our code a little bit further
            -- right
            paragraphIndent0
            hSymmetricGradient (rgbHex bgHex) quoteBg 80
            marginBottom (em 1)
            paddingTop (em 1)
          pre ? do
            noMargin
            vh padding 0 $ px 0
            code ? do
              "-moz-tab-size" -: "4"
              "-o-tab-size" -: "4"
              "tab-size" -: "4"
              display block
              -- we indent a little bit more here compared to
              -- table.sourceCode, because here we have to
              -- compensate for the fact that we don't have line
              -- numbers to push our code a little bit further
              -- right
              paragraphIndent'
              hSymmetricGradient (rgbHex bgHex) codeBg 80
              ev borderRadius (px 0)
              "border-style" -: "none"
              marginBottom (em 1)
              paddingTop (em 1)
              paddingBottom (em 1)
              fontWeight normal
          table ? do -- code with line numbers
            ".sourceCode" & do
              "-moz-tab-size" -: "4"
              "-o-tab-size" -: "4"
              "tab-size" -: "4"
              noMargin
              marginBottom (em 1)
              borderSpacing2 (px 0) (px 1)
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
            -- table for list of all blog posts
            ".posts-index" & do
              headerIndent
              marginBottom $ px 0
              paddingBottom (em 1)
              thead ? do
                tr ? do
                  th ? do
                    "border-style" -: "none"
                    ".label" & do
                      textAlign $ alignSide sideCenter
                    ".title" & do
                      paddingLeft $ em 1
                      textAlign $ alignSide sideLeft
              tr ? do
                td ? do
                  vh padding 0 $ em 0.5
                  "border-style" -: "none"
                  "vertical-align" -: "text-top"
                  ".date" & do
                    whiteSpace nowrap
                  ".bytes" & do
                    textAlign $ alignSide sideRight
                    whiteSpace nowrap
                  code ? do
                    fontWeight normal
        "#footer" & do
          -- Margins of adjacent elements are *overlap*, unlike
          -- padding. We use a 1em top-side margin here, because,
          -- e.g., code listings sometimes have a bottom margin, and
          -- if the blog post ends with a code listing, we don't want
          -- to render both the listing's bottom spacing and the top
          -- spacing of the footer.
          marginTop (em 1)
          paddingBottom (em 1)
          fontSizeCustom Clay.Font.medium
          textAlign $ alignSide sideCenter
  where
  div' = Clay.div
  cPageWidth :: Double
  cPageWidth = 900
  codeBgHex :: Int
  codeBgHex = bgHex
  quoteBgHex :: Int
  quoteBgHex = bgHex - 0x0f0f0f
  footnotesBgHex :: Int
  footnotesBgHex = bgHex - 0x0f0f0f
  bgHex :: Int
  bgHex = 0xffffff
  textColor :: Color
  textColor = rgbHex 0x000000
  codeBg :: Color
  codeBg = rgbHex codeBgHex
  footnotesBg :: Color
  footnotesBg = rgbHex footnotesBgHex
  quoteBg :: Color
  quoteBg = rgbHex quoteBgHex
  lineNumHex :: Int
  lineNumHex = bgHex - 0x555555
  linkHex :: Int
  linkHex = 0x0033ff
  linkVisitedHex :: Int
  linkVisitedHex = 0x007033
  headerIndent = "padding-left" -: "12%"
  headerIndentRight = "padding-right" -: "12%"
  paragraphIndent0 = do
    "padding-left" -: "6%"
    paragraphIndentRight
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
  sourceCodeMarkdownNumberlines _ = ".sourceCode" & do
    headerIndent
    paddingBottom (em 0.5)
    tr ? do
      td ? do
        ".lineNumbers" & do
          pre ? do
            noMargin
            color (rgbHex lineNumHex)
            textAlign $ alignSide sideRight
        ".sourceCode" & do
          paddingLeft $ px 0
          pre ? do
            noMargin
            code ? do
              noMargin
              padding 0 0 0 (em 1)
  uBlackSquare = 0x25a0
  uWhiteSquare = 0x25a1
  uDoubleSquare = 0x25a3
  uBlackDiamond = 0x25c6
  uWhiteDiamond = 0x25c7
  uDoubleDiamond = 0x25c8
  addHeadingSymbol hex = before & do
    CT.content $ stringContent symbol
    position relative
    where
    symbol = T.toStrict
      . T.pack
      $ (toEnum hex)
      : (toEnum 0x00a0) -- nonbreaking space character
      : (toEnum 0x00a0) -- nonbreaking space character
      : []
  hSymmetricGradient colorSides colorMiddle middleWidth
    = backgroundImage
    $ linearGradient
      (straight sideRight)
      [ (colorSides, pct 0)
      , (colorMiddle, pct $ 50 - middleWidth / 2)
      , (colorMiddle, pct $ 50 + middleWidth / 2)
      , (colorSides, pct 100)]
-- | A horizontal/vertical size helper. It accepts a function and two sizes for
-- the horizontal and vertical parts. E.g., instead of calling
--     padding (px 6) (px 10) (px 6) (px 10)
-- you can simply do
--    vh padding (px 6) (px 10)
-- to save some keystrokes and decrease the chance of typos.
vh :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Size a -> Css
vh f x y = f x y x y

-- | Like "vh", but uses the same size for *everything*.
ev :: (Size a -> Size a -> Size a -> Size a -> Css) -> Size a -> Css
ev f x = f x x x x

noMargin :: Css
noMargin = margin 0 0 0 $ px 0
