{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Lazy as M
import System.FilePath.Posix
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))
import Text.Pandoc.Definition
import Hakyll

main :: IO ()
main = hakyll $ do
	-- Build tags
	tags <- buildTags "post/*" (fromCapture "tag/*.html")

	tagsRules tags $ \tag pattern -> do
		let
			title = "&ldquo;" ++ tag ++ "&rdquo;"
		route idRoute
		compile $ do
			list <- postList tags pattern recentFirst
			makeItem ""
				>>= loadAndApplyTemplate "template/archive.html" (mconcat
					[ constField "body" list
					, archiveCtx tags
					, defaultContext
					])
				>>= loadAndApplyTemplate "template/default.html" (mconcat
					[ constField "title" title
					, mathCtx
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
				(unixFilter "cabal" ["exec", "runghc"])

	-- Add some default pages
	match (fromList ["about.md", "art.md", "code.md", "papers.md"]) $ do
		route   $ setExtension "html"
		compile $ pandocCompiler
			>>= loadAndApplyTemplate "template/default.html" (mconcat
				[ mathCtx
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
		]

	match "post/*.md" $ do
		route $ setExtension "html"
		compile $ pandocCompilerWithTransformM
			defaultHakyllReaderOptions
			pandocOptions
			transformer
			>>= loadAndApplyTemplate "template/post.html"    (tagsCtx tags)
			>>= saveSnapshot "content"
			>>= loadAndApplyTemplate "template/default.html" (mconcat
				[ mathCtx
				, tagsCtx tags
				])
			>>= relativizeUrls

	create ["archive.html"] $ do
		route idRoute
		compile $ do
			posts <- loadAll "post/*"
			sorted <- recentFirst posts
			itemTpl <- loadBody "template/post-item.html"
			list <- applyTemplateList itemTpl postCtx sorted
			makeItem list
				>>= loadAndApplyTemplate "template/archive.html"
					(archiveCtx tags)
				>>= loadAndApplyTemplate "template/default.html" (mconcat
					[ mathCtx
					, archiveCtx tags
					])
				>>= relativizeUrls

	create ["index.html"] $ do
		route idRoute
		compile $ do
			posts <- loadAll "post/*"
			sorted <- fmap (take 3) $ recentFirst posts
			itemTpl <- loadBody "template/post-item.html"
			list <- applyTemplateList itemTpl postCtx sorted
			makeItem list
				>>= loadAndApplyTemplate "template/index.html" (homeCtx list)
				>>= loadAndApplyTemplate "template/default.html" (mconcat
					[ mathCtx
					, homeCtx list
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
	, defaultContext
	]

archiveCtx :: Tags -> Context String
archiveCtx tags = mconcat
	[ constField "title" "Archive"
	, field "taglist" (\_ -> renderTagList tags)
	, defaultContext
	]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
	[ tagsField "prettytags" tags
	, postCtx
	]

homeCtx :: String -> Context String
homeCtx list = mconcat
	[ constField "post" list
	, constField "title" "Home"
	, defaultContext
	]

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
	metadata <- getMetadata $ itemIdentifier item
	return $ if (M.member "mathjax" metadata)
		then concat
			[ "<script src=\""
			, "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
			, "?config=TeX-AMS-MML_HTMLorMML\"></script>"
			]
		else ""

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
		let
			codeLang = case takeExtensions fp of
				".c" -> ["c"]
				".el" -> ["commonlisp"]
				".hs" -> ["haskell"]
				".rb" -> ["ruby"]
				".sh" -> ["bash"]
				".xorg.conf" -> ["xorg"]
				_ -> []
			httpTarget = "/code/" ++ fp
			fn = takeFileName fp
			attr = ("", ["numberLines"] ++ codeLang, [("input", "code/" ++ fp)])
		raw <- unsafeCompiler . readFile $ "code/" ++ fp
		return
			( True
			,
				( Div ("", ["code-and-raw"], [])
					[ CodeBlock attr raw
					, Div ("", ["raw-link"], [])
						[ Plain
							[ RawInline
								"html" $
								unwords
									[ "<a"
									, " class=\"raw\""
									, " href="
									, dquote httpTarget
									, " mimetype=text/plain"
									, ">"
									, fn
									, "</a>"
									]
							]
						]
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
	, feedRoot = "http://listx.github.io"
	}

dquote :: String -> String
dquote str = "\"" ++ str ++ "\""
