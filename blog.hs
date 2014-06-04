{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Lazy as M
import Data.Monoid (mconcat)
import Text.Pandoc (WriterOptions (..), HTMLMathMethod (MathJax))
import Hakyll

main :: IO ()
main = hakyll $ do
	-- Build tags
	tags <- buildTags "post/*" (fromCapture "tag/*.html")

	tagsRules tags $ \tag pattern -> do
		let
			title = "Posts tagged &ldquo;" ++ tag ++ "&rdquo;"
		route idRoute
		compile $ do
			list <- postList tags pattern recentFirst
			makeItem ""
				>>= loadAndApplyTemplate "template/archive.html" (mconcat
					[ constField "body" list
					, defaultContext
					])
				>>= loadAndApplyTemplate "template/default.html" (mconcat
					[ constField "title" title
					, mathCtx
					, defaultContext
					])
				>>= relativizeUrls

	-- Add images
	match "*.png" $ do
		route idRoute
		compile copyFileCompiler

	-- Add raw CSS
	match "css/*.css" $ do
		route   idRoute
		compile compressCssCompiler

	-- Add Clay-based css
	match "css/*.hs" $ do
		route $ setExtension "css"
		compile $ getResourceString >>= withItemBody (unixFilter ".cabal-sandbox/bin/cabal" ["exec", "runghc"])

	-- Add some default pages
	match (fromList ["about.md", "code.md"]) $ do
		route   $ setExtension "html"
		compile $ pandocCompiler
			>>= loadAndApplyTemplate "template/default.html" (mconcat
				[ mathCtx
				, defaultContext
				])
			>>= relativizeUrls

	-- Add images
	match "img/*" $ do
		route idRoute
		compile copyFileCompiler

	-- Add files
	match "file/*" $ do
		route idRoute
		compile copyFileCompiler

	match "post/*" $ do
		route $ setExtension "html"
		compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
			>>= loadAndApplyTemplate "template/post.html"    (tagsCtx tags)
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
				>>= loadAndApplyTemplate "template/archive.html" archiveCtx
				>>= loadAndApplyTemplate "template/default.html" (mconcat
					[ mathCtx
					, archiveCtx
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
				>>= loadAndApplyTemplate "template/index.html" (homeCtx tags list)
				>>= loadAndApplyTemplate "template/default.html" (mconcat
					[ mathCtx
					, homeCtx tags list
					])
				>>= relativizeUrls

	match "template/*" $ compile templateCompiler
	where
	pandocOptions :: WriterOptions
	pandocOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }

postCtx :: Context String
postCtx = mconcat
	[ dateField "date" "%Y-%m-%d"
	, defaultContext
	]

archiveCtx :: Context String
archiveCtx = mconcat
	[ constField "title" "Archive"
	, defaultContext
	]

tagsCtx :: Tags -> Context String
tagsCtx tags = mconcat
	[ tagsField "prettytags" tags
	, postCtx
	]

homeCtx :: Tags -> String -> Context String
homeCtx tags list = mconcat
	[ constField "post" list
	, constField "title" "Home"
	, field "taglist" (\_ -> renderTagList tags)
	, defaultContext
	]

mathCtx :: Context a
mathCtx = field "mathjax" $ \item -> do
	metadata <- getMetadata $ itemIdentifier item
	return $ if (M.member "mathjax" metadata)
		then "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
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
