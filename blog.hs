{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Char hiding (Space)
import Data.Time.Calendar
import Data.Time.LocalTime
import Hakyll
import System.FilePath.Posix
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))
import Text.Pandoc.Definition
import qualified Text.Printf as TF

main :: IO ()
main = hakyll $ do
  -- Build tags
  tags <- buildTags "post/*" (fromCapture "tag/*.html")
  (currentYear, _, _) <- toGregorian . localDay . zonedTimeToLocalTime <$> preprocess (getZonedTime)
  tagsRules tags $ \tag pattern -> do
    let
      title = "&ldquo;" <> tag <> "&rdquo;"
    route idRoute
    compile $ do
      list <- postList tags pattern recentFirst
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
        (unixFilter "cabal"
        [ "exec"
        , "--"
        , "runghc"
        , "--"
        , "-fno-warn-tabs"
        ])

  -- Add some default pages
  match (fromList ["about.md", "art.md", "code.md", "papers.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "template/default.html" (mconcat
        [ copyrightCtx currentYear
        , defaultContext
        ])
      >>= relativizeUrls

  -- Add static content
  mapM_ (flip match (route idRoute >> compile copyFileCompiler))
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

  match (fromRegex "post/[^/]+\\.(md|org)$") $ do
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
  [ constField "title" "Linus's Blog"
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
postList tags pattern sortFilter = do
  posts <- sortFilter =<< loadAll pattern
  itemTpl <- loadBody "template/post-item.html"
  applyTemplateList itemTpl (tagsCtx tags) posts

fileNameField :: String -> Context String
fileNameField key = field key $ \item -> do
  return . toFilePath $ itemIdentifier item

bytesField :: String -> Context String
bytesField key = field key $ \item -> do
  return . showKChars . length . filter isAlphaNum $ itemBody item
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
  (BulletList xs) -> return . maybeBullets =<< mapM (mapM bList) xs
  _ -> return [block]
  where
  bList :: Block -> Compiler (Bool, Block)
  bList (Plain [(Str "i"), Space, (Str fp)]) = do
    raw <- unsafeCompiler . readFile $ "code/" <> fp
    let
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
      fn = takeFileName fp
      (lineCntClass, bulletSpaces)
        | length (lines raw) < 10 = ("10", s 0)
        | length (lines raw) < 100 = ("100", s 1)
        | length (lines raw) < 1000 = ("1000", s 2)
        | otherwise = ("10000", s 3)
        where
        s = concat . flip replicate "&nbsp;"
      attr = ("", ["numberLines"] <> codeLang, [("input", "code/" <> fp)])
      filename_link_raw =
        RawInline
          "html" $
          unwords
            [ "<table class=\"sourceCode numberLines noPaddingBottom\"><tbody><tr class=\"sourceCode\"><td class=\"lineNumbers\"><pre>" <> bulletSpaces <> "■</pre></td><td class=\"sourceCode\"><pre><code><a"
            , "class=\"raw\""
            , "href="
            , dquote httpTarget
            , "mimetype=text/plain"
            , ">" <> fn <> "</a></code></pre></td></tr></tbody></table>"
            ]
    return
      ( True
      ,
        ( Div ("", ["code-and-raw", "lineCntMax" <> lineCntClass], [])
          [ Div ("", ["raw-link", "sourceCode"], [])
            [ Plain
              [ filename_link_raw
              ]
            ]
          , CodeBlock attr raw
          ]
        )
      )
  bList x = return (False, x)
  maybeBullets [] = [BulletList []]
  maybeBullets xss = case head xss of
    ((True, _):_) -> concatMap (map snd) xss
    _ -> [BulletList $ map (map snd) xss]

atomFeedConf :: FeedConfiguration
atomFeedConf = FeedConfiguration
  { feedTitle = "Linus's Blog"
  , feedDescription = "The latest blog posts from Linus (listx on Github)!"
  , feedAuthorName  = "Linus Arver"
  , feedAuthorEmail = ""
  , feedRoot = "http://funloop.org"
  }

dquote :: String -> String
dquote str = "\"" <> str <> "\""
