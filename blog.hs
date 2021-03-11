{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

import Data.Char hiding (Space)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Void
import Hakyll
import System.FilePath.Posix
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))
import Text.Pandoc.Definition
import Text.Printf qualified as TF

import Numeric.Natural
import Text.Megaparsec
  ( choice
  , errorBundlePretty
  , parse
  , Parsec
  , ParseErrorBundle
  , some
  , try
  )
import Text.Megaparsec.Char
  ( char
  , digitChar
  )

main :: IO ()
main = hakyll $ do
  -- Build tags
  tags <- buildTags "post/*" (fromCapture "tag/*.html")
  (currentYear, _, _) <- toGregorian . localDay . zonedTimeToLocalTime <$> preprocess getZonedTime
  tagsRules tags $ \tag pat -> do
    let
      title = "&ldquo;" <> tag <> "&rdquo;"
    route idRoute
    compile $ do
      list <- postList tags pat recentFirst
      makeItem ""
        >>= loadAndApplyTemplate "template/index.html" (mconcat
          [ constField "body" list
          , homeCtx tags
          , defaultContext
          ])
        >>= loadAndApplyTemplate "template/default.html" (mconcat
          [ constField "title" title
          , copyrightCtx currentYear
          , defaultContext
          ])
        >>= relativizeUrls

  -- Add raw CSS
  match "css/*.css" $ do
    route   idRoute
    compile compressCssCompiler

  -- Add Clay-based css
  match "css/*.hs" $ do
    route $ setExtension "css"
    compile $ getResourceString
      >>= withItemBody
        (unixFilter "make"
        [ "-s"
        , "gen-css"
        ])

  -- Add some default pages
  match (fromList ["about.org", "etc.org"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompilerWithTransformM
      defaultHakyllReaderOptions
      pandocOptions
      transformer
      >>= loadAndApplyTemplate "template/default.html" (mconcat
        [ copyrightCtx currentYear
        , defaultContext
        ])
      >>= relativizeUrls

  -- Add static content
  mapM_ (`match` (route idRoute >> compile copyFileCompiler))
    [ "CNAME"
    -- Although not tailored to the actual deployed site itself, it still
    -- has some rules that make it easier to git add/push new content.
    , ".gitignore"
    , "*.png"
    , "code/**"
    , "img/**"
    , "file/**"
    , "misc.js"
    ]

  match (fromRegex "post/[^/]+\\.org$") $ do
    route $ setExtension "html"
    compile $ pandocCompilerWithTransformM
      defaultHakyllReaderOptions
      pandocOptions
      transformer
      >>= loadAndApplyTemplate "template/post.html"    (tagsCtx tags)
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "template/default.html" (mconcat
        [ copyrightCtx currentYear
        , tagsCtx tags
        ])
      >>= relativizeUrls

  create ["index.html"] $ do
    route idRoute
    compile $ do
      posts <- loadAll "post/*"
      sorted <- recentFirst posts
      itemTpl <- loadBody "template/post-item.html"
      list <- applyTemplateList itemTpl postCtx sorted
      makeItem list
        >>= loadAndApplyTemplate "template/index.html"
          (homeCtx tags)
        >>= loadAndApplyTemplate "template/default.html" (mconcat
          [ copyrightCtx currentYear
          , homeCtx tags
          ])
        >>= relativizeUrls

  match "template/*" $ compile templateCompiler

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let
        feedCtx = mconcat
          [ postCtx
          , bodyField "description"
          ]
      posts <- fmap (take 10)
        . recentFirst
        =<< loadAllSnapshots "post/*" "content"
      renderAtom atomFeedConf feedCtx posts
  where
  pandocOptions :: WriterOptions
  pandocOptions = defaultHakyllWriterOptions
    {writerHTMLMathMethod = MathJax ""}

postCtx :: Context String
postCtx = mconcat
  [ dateField "date" "%Y-%m-%d"
  , fileNameField "filename"
  , bytesField "bytes"
  , defaultContext
  ]

homeCtx :: Tags -> Context String
homeCtx tags = mconcat
  [ constField "title" "Linus’s Blog"
  , field "taglist" (\_ -> renderTagList tags)
  , defaultContext
  ]

copyrightCtx :: Integer -> Context String
copyrightCtx currentYear = mconcat
  [ constField "copyright"
    $ unwords
      [ "Copyright (C) 2013-" <> show currentYear <> " Linus Arver."
      , "All rights reserved."
      ]
  ]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
  [ tagsField "prettytags" tags
  , postCtx
  ]

postList
  :: Tags
  -> Pattern
  -> ([Item String] -> Compiler [Item String])
  -> Compiler String
postList tags pat sortFilter = do
  posts <- sortFilter =<< loadAll pat
  itemTpl <- loadBody "template/post-item.html"
  applyTemplateList itemTpl (tagsCtx tags) posts

fileNameField :: String -> Context String
fileNameField key = field key $
  \item -> return . toFilePath $ itemIdentifier item

bytesField :: String -> Context String
bytesField key = field key $
  \item -> return . showKChars . length . filter isAlphaNum
    $ itemBody item
  where
  showKChars :: Int -> String
  showKChars n = TF.printf "%.1f" (fromIntegral n / 1000.0 :: Double)

transformer :: Pandoc -> Compiler Pandoc
transformer (Pandoc m bs0) = do
  bs1 <- mapM cbExpandRawInput bs0
  return . Pandoc m $ concat bs1

-- We allow the user to do
--
-- - i foo/bar.hs
--
-- or
--
-- - i foo/bar.hs
-- - i baz/quux.hs (multiple items)
--
-- in a file, and make it expand to the equivalent
--
-- ```{.numberLines .haskell}
-- [CONTENTS OF "foo/bar.hs"]
-- ```
--
-- form, but also with a hyperlink to the file "foo/bar.hs". This is by far much
-- easier to write in actual blog posts.
cbExpandRawInput :: Block -> Compiler [Block]
cbExpandRawInput block = case block of
  (BulletList xs) -> maybeBullets <$> mapM (mapM bList) xs
  _ -> return [block]
  where
  bList :: Block -> Compiler (Bool, Block)
  -- "0" range means the entire file.
  bList (Plain [Str "i", Space, Str fp]) = injectRaw (T.unpack fp) "0"
  bList (Plain [Str "i", Space, Str fp, Space, Str range]) = injectRaw (T.unpack fp) range
  bList x = return (False, x)
  maybeBullets [] = [BulletList []]
  maybeBullets xss = case head xss of
    ((True, _):_) -> concatMap (map snd) xss
    _ -> [BulletList $ map (map snd) xss]
  injectRaw fp range = do
    (raw :: T.Text) <- unsafeCompiler . T.readFile $ "code/" <> fp
    let
      (a, b) = case parseRange range of
        Right res -> res
        Left err -> error $ errorBundlePretty err
      -- Capture a portion (or all of) the lines found in "raw".
      rawSnippet
        -- Retrieve all lines.
        | a == 0 && b == 0
          = raw
        -- Only retrieve 1 line.
        | a == b
          = T.unlines
          . take 1
          . drop (fromIntegral a - 1)
          $ T.lines raw
        -- Retrieve a range of lines, a to b.
        | otherwise
          = T.unlines
          . (take ((fromIntegral b - fromIntegral a) + 1))
          . drop (fromIntegral a - 1)
          $ T.lines raw
      codeLang = case takeExtensions fp of
        ".c" -> ["c"]
        ".el" -> ["commonlisp"]
        ".hs" -> ["haskell"]
        ".py" -> ["python"]
        ".rb" -> ["ruby"]
        ".sh" -> ["bash"]
        ".xorg.conf" -> ["xorg"]
        _ -> []
      httpTarget = "/code/" <> fp
      filename = takeFileName fp
      lineCntClass
        | length (T.lines rawSnippet) < 10 = "10"
        | length (T.lines rawSnippet) < 100 = "100"
        | length (T.lines rawSnippet) < 1000 = "1000"
        | otherwise = "10000"
      attr = ("", ["numberLines"] <> codeLang, [("input", T.pack $ "code/" <> fp)])
      filename_link_raw =
        RawInline "html" . T.pack $
          unwords
            [ "<p>"
            , "<a class=\"raw\" href="
              <> dquote httpTarget
              <> "mimetype=text/plain>"
              <> "<code>"
                <> filename
                <> "</code>"
              <> "</a>"
            , "</p>"
            ]
    return
      ( True
      , Div ("", ["code-and-raw", "lineCntMax" <> lineCntClass], [])
        [ CodeBlock attr rawSnippet
        , Div ("", ["raw-link", "sourceCode"], [])
          [ Plain
            [ filename_link_raw
            ]
          ]
        ]
      )

type Parser = Parsec Void T.Text

parseRange :: T.Text -> Either (ParseErrorBundle T.Text Void) (Natural, Natural)
parseRange = parse pRange ""

-- Parse a range tuple, that looks like "<a>" or "<a>-<b>", where "a" and "b"
-- are both natural numbers and a <= b. If b is not given, then the range
-- "<a>-<a>" is implied.
pRange :: Parser (Natural, Natural)
pRange = choice [try parseAB, parseA]
  where
  parseAB :: Parser (Natural, Natural)
  parseAB = do
    a <- some digitChar
    _ <- char '-'
    b <- some digitChar
    let
      an = read a
      bn = read b
    if
      | an > bn
        -> fail $
          unwords
          [ show a
          , "is greater than"
          , show b
          ]
      -- If a valid range starts with 0, use 1-based indexing for the first
      -- line.
      | an == 0
        -> pure (1, bn)
      | otherwise
        -> pure (an, bn)
  parseA :: Parser (Natural, Natural)
  parseA = do
    a <- some digitChar
    let
      an = read a
    pure (an, an)

atomFeedConf :: FeedConfiguration
atomFeedConf = FeedConfiguration
  { feedTitle = "Linus’s Blog"
  , feedDescription = "The latest blog posts from Linus (listx on Github)!"
  , feedAuthorName  = "Linus Arver"
  , feedAuthorEmail = ""
  , feedRoot = "http://funloop.org"
  }

dquote :: String -> String
dquote str = "\"" <> str <> "\""
