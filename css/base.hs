{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import Data.Bits
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Clay hiding (vh)
import qualified Clay.Display as CD
import Clay.Font
import qualified Clay.Text as CT
import qualified Clay.Stylesheet as CS

import Skylighting
  ( styleToCss
  , haddock
  )

main :: IO ()
main = T.putStrLn $ T.unlines
  [ "/* base.hs: Inject Skylighting-generated CSS */"
  , T.pack $ styleToCss haddock
  -- NOTE: It is important to put the Clay-generated stuff last, so that we can
  -- tweak the default code block styles created by Skylighting.
  , "/* base.hs: Inject Clay-generated CSS */"
  , renderWith pretty [] myStylesheet
  ]

rgbHex :: Int -> Color
rgbHex rgb'
  | rgb' > 0xffffff || rgb' < 0 = error "invalid hex range"
  | otherwise = rgb rr gg bb
  where
  rr = fromIntegral $ shiftR rgb' 16 .&. 0xFF
  gg = fromIntegral $ shiftR rgb' 8 .&. 0xFF
  bb = fromIntegral $ rgb' .&. 0xFF

-- Clay doesn't have a "border-image" key yet, so make one up here.
newtype BorderImage = BorderImage Value
  deriving (Val, Other, Inherit, None)
-- The "border-image" key can take on a property that looks like a
-- BackgroundImage value, so use that directly here.
borderImage :: BackgroundImage -> Css
borderImage = CS.key "border-image"

myStylesheet :: Css
myStylesheet = do
  html ? do
    noMargin
    ev padding $ px 0
    backgroundColor $ rgbHex bgHex
    color textColor
    body ? do
      overflowY scroll
      fontFamily ["Source Serif Pro"] [serif]
      width (px cPageWidth)
      vh margin (px 0) auto
      a ? do
        outline none (px 1) (rgbHex 0x000000)
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
        fontSizeCustom Clay.Font.small
        border (px 1) solid $ rgbHex 0x000000
        margin 0 auto (em 1) auto
        width (pct 76)
      div' ? do
        ".center" & do
          textAlign $ alignSide sideCenter
      div' ? do
        "#sticky-title" & do
          "top" -: "0"

          -- This places this element above MathJax formulas and our custom
          -- bullets for h1 (and h2, h3, etc) headings.
          zIndex 2

          hSymmetricGradient (rgbHex bgHex) (rgbHex 0xe0f8f2) 80
          hSymmetricGradientBorder (rgbHex bgHex) (rgbHex 0x37a68a) 80 (px 1)
          vh padding (em 0.5) 0
          marginBottom (em 2)
          h1 ? do
            paragraphIndent
            textAlign $ alignSide sideCenter
          -- See https://stackoverflow.com/a/62366856/437583 for more on box
          -- shadows (with/without attenuation near the corners).
          boxShadow
            [ bsColor (rgbHex 0xaaaaaa)
              $ shadowWithSpread
                (px 0)      -- horizontal offset (0 means the "sun" is directly overhead)
                (px 30)     -- vertical offset (the bigger the lower the shadow)
                (px 25)     -- blur radius
                (px (-30))  -- spread (overall shadow size)
            ]
      div' ? do
        "#header" & do
          vh padding (em 0.5) (em 0)
          textAlign $ alignSide sideCenter
          fontSize (pt 18)
        "#content" & do
          backgroundColor $ rgbHex bgHex
          paddingBottom (px 0)
          div' ? do
            ".canvas-centered" & do
              display grid
              "justify-items" -: "center"
              vh margin (em 1) auto
              canvas ? do
                "image-rendering" -: "pixelated"
                "image-rendering" -: "crisp-edges"
                "image-rendering" -: "-moz-crisp-edges"
                "image-rendering" -: "-webkit-crisp-edges"
                ".active-pixel-canvas" & do
                  zIndex 1
                ".layer" & do
                  "grid-area" -: "1/1"
            ".canvas-tooltip" & do
              position absolute
              backgroundColor $ rgbHex bgHex
              vh padding (px 4) (px 4)
              border (px 2) solid $ rgbHex 0x000000
              -- Start out invisible from the user. JS activates it to be
              -- visible with opacity "1" when the desired mouse event fires up.
              opacity 0
          h1 <> h2 <> h3 <> h4 <> h5 <> h6 ? do
            noMargin
            headerIndent
            paragraphIndentRight
            paddingBottom (em 0.5)
            fontWeight normal
            code ? do
              vh margin (px 0) (px 4)
            -- All headers are linked with a "#" anchor tag by JS. We want the
            -- #sticky-title div to not be in the way (on top of these anchors)
            -- when we click on them. The scroll-margin-top leaves room for the
            -- #sticky-title div. See
            -- https://stackoverflow.com/questions/4086107/fixed-page-header-overlaps-in-page-anchors.
            paddingForStickyTitle
          h1 ? do
            addHeadingSymbol uDoubleSquare
            -- For the title of the page, center-align it.
            ".center" & do
              noMargin
              paddingTop (em 0)
              paddingBottom (em 0.5)
              fontSize (pt 30)
              fontWeight bold
              headerIndentRight
              textAlign $ alignSide sideCenter
              textDecoration none
              before & do
                CT.content (none :: Content)
            "#page-title" & do
              paddingBottom (px 0)
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
              borderLeft (px 1) solid (rgbHex 0x000000)
          table ? do
            ".ul" & do
              borderCollapse separate
              td ? do
                "border-style" -: "none"
          div' ? do
            ".info" & do
              paddingBottom (em 1)
          div' ? do
            -- This is when we use our custom 'import source code'
            -- syntax with '- i <filename>'.
            --
            -- This is the overall block.
            ".code-and-raw" & do
              backgroundColor $ rgbHex bgHex
              marginBottom (em 1)
              paddingLeft (px 0)
              div' ? do
                ".sourceCode" & do
                  hSymmetricGradient (rgbHex bgHex) codeBg 80
                  noMargin
                  pre ? do
                    ".numberSource" & do
                      "border-style" -: "none"
                    code ? do
                      backgroundImage none
                      marginBottom (px 0)
                      Clay.span ? do
                        -- Line numbers links.
                        paddingForStickyTitle
                -- One line for the raw link to the injected source code.
                ".raw-link" & do
                  hSymmetricGradient (rgbHex bgHex) codeLinkBg 80
                  hSymmetricGradientBorder (rgbHex bgHex) (rgbHex 0x888888) 80 (px 1)
                  borderTopWidth (px 0)
                  hr ? do
                    border (px 1) solid $ rgbHex 0xcccccc
                    marginBottom (px 0)
                    width (pct 76)
                  p ? do
                    vh padding (em 0.5) 0
                    textAlign $ alignSide sideCenter
                    a ? do
                      ".raw" & do
                        marginLeft $ em 0
                      visited & do
                        color (rgbHex linkHex)
                      fontWeight bold
            -- Code inside '#+begin_src' in org-mode.
            ".sourceCode" & do
              codeBlock
          -- Code inside '#+begin_example in org-mode.
          codeBlock
          ol ? do
            noMargin
            paragraphListIndent
            paddingBottom (em 1)
            p ? do
              ev padding $ px 0
            li ? do
              p ? do
                ev padding $ px 0
                paddingBottom (em 1)
          ul ? do
            noMargin
            paragraphListIndent
            paddingBottom (em 1)
            li ? do
              p ? do
                ev padding $ px 0
                paddingBottom (em 1)
            ul ? do
              noMargin
              paragraphIndent0
              paddingBottom $ px 0
          -- Make MathJax equation link targets (anchors) reserve room for the
          -- #sticky-title div at the top. For some reason in Firefox we get a
          -- little extra padding than in Chrome. But we can live with that.
          Clay.span ? do
            ".mtd" & do
              paddingForStickyTitle
          p ? do
            noMargin
            paddingBottom (em 1)
            paragraphIndent
            textAlign justify
            a ? do
              -- This is for anchor links from footnotes at the bottom that point
              -- back to where the footnote came from.
              paddingForStickyTitle

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
          blockquote ? do
            noMargin
            vh padding 0 $ px 0
            "-moz-tab-size" -: "4"
            "-o-tab-size" -: "4"
            "tab-size" -: "4"
            hSymmetricGradient (rgbHex bgHex) quoteBg 80
            hSymmetricGradientBorder (rgbHex bgHex) (rgbHex 0x8ece8e) 80 (px 1)
            marginBottom (em 1)
            paddingTop (em 1)
          table ? do
            tbody ? do
              tr ? do
                td ? do
                  ".centered" & do
                    textAlign $ alignSide sideCenter
            -- table for list of all blog posts
            ".posts-index" & do
              "margin-left" -: "12%"
              "margin-right" -: "12%"
              paddingBottom (em 1)
              thead ? do
                tr ? do
                  th ? do
                    "border-style" -: "none"
                    ".label" & do
                      textAlign $ alignSide sideCenter
                    ".size" & do
                      paddingLeft (em 0.5)
                    ".title" & do
                      paddingLeft (em 0.5)
                      textAlign $ alignSide sideLeft
              tbody ? do
                tr ? do
                  td ? do
                    "border-style" -: "none"
                    "vertical-align" -: "text-top"
                    ".date" & do
                      vh padding (px 0) (px 0)
                      whiteSpace nowrap
                    ".bytes" & do
                      paddingLeft (em 1)
                      textAlign $ alignSide sideRight
                      whiteSpace nowrap
                    code ? do
                      fontWeight normal
          section ? do
            ".footnotes" & do
              ol ? do
                paragraphIndent
        "#footer" & do
          -- Margins of adjacent elements are *overlap*, unlike
          -- padding. We use a 1em top-side margin here, because,
          -- e.g., code listings sometimes have a bottom margin, and
          -- if the blog post ends with a code listing, we don't want
          -- to render both the listing's bottom spacing and the top
          -- spacing of the footer.
          marginTop (em 1)
          paddingBottom (em 2)
          fontSizeCustom Clay.Font.medium
          textAlign $ alignSide sideCenter
          p ? do
            paddingTop (em 1)
            ev margin (px 0)
          img ? do
            "#brand-icon" & do
              -- Lifted from https://stackoverflow.com/a/14068216/437583
              "image-rendering" -: "optimizeSpeed"
              "image-rendering" -: "-moz-crisp-edges"
              "image-rendering" -: "-o-crisp-edges"
              "image-rendering" -: "-webkit-optimize-contrast"
              "image-rendering" -: "pixelated"
              "image-rendering" -: "optimize-contrast"
              "-ms-interpolation-mode" -: "nearest-neighbor"
              -- The icon is 13px wide, so the width has to be an increment of
              -- 13, in this case 39 = 13 * 3.
              width (px 39)
  where
  div' = Clay.div
  paddingForStickyTitle = "scroll-margin-top" -: "110px"
  cPageWidth :: Number
  cPageWidth = 900
  codeBgHex :: Int
  codeBgHex = bgHex - 0x0f0f0f
  codeLinkBgHex :: Int
  codeLinkBgHex = bgHex - 0x1f1f1f
  quoteBgHex :: Int
  quoteBgHex = 0xddffdd
  footnotesBgHex :: Int
  footnotesBgHex = bgHex - 0x0f0f0f
  bgHex :: Int
  bgHex = 0xffffff
  textColor :: Color
  textColor = rgbHex 0x000000
  codeBg :: Color
  codeBg = rgbHex codeBgHex
  codeLinkBg :: Color
  codeLinkBg = rgbHex codeLinkBgHex
  footnotesBg :: Color
  footnotesBg = rgbHex footnotesBgHex
  quoteBg :: Color
  quoteBg = rgbHex quoteBgHex
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
  paragraphListIndent = do
    "padding-left" -: "18%"
    paragraphIndentRight
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
      $ toEnum hex
      : toEnum 0x00a0 -- nonbreaking space character
      : toEnum 0x00a0 -- nonbreaking space character
      : []
  hSymmetricGradientBorder colorSides colorMiddle middleWidth w = do
    borderImage $ horizontalGradient colorSides colorMiddle middleWidth
    "border-image-slice" -: "1"
    borderWidth w
    borderStyle solid
  hSymmetricGradient colorSides colorMiddle middleWidth
    = backgroundImage $ horizontalGradient colorSides colorMiddle middleWidth
  horizontalGradient colorSides colorMiddle middleWidth = linearGradient
    (straight sideRight)
    [ (colorSides, pct 0)
    , (colorMiddle, pct $ 50 - middleWidth / 2)
    , (colorMiddle, pct $ 50 + middleWidth / 2)
    , (colorSides, pct 100)]
  codeBlock = do
    noMargin
    pre ? do
      "border-style" -: "none"
      noMargin
      vh padding 0 $ px 0
      code ? do
        "-moz-tab-size" -: "4"
        "-o-tab-size" -: "4"
        "tab-size" -: "4"
        display block
        hSymmetricGradient (rgbHex bgHex) codeBg 80
        hSymmetricGradientBorder (rgbHex bgHex) (rgbHex 0x888888) 80 (px 1)
        ev borderRadius (px 0)
        marginBottom (em 1)
        paragraphIndent
        paddingTop (em 1)
        paddingBottom (em 1)
        fontWeight normal
        Clay.span ? do
          -- Line numbers links.
          paddingForStickyTitle

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
