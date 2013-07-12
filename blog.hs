{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend, mconcat)
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
				>>= loadAndApplyTemplate "template/archive.html"
					(constField "title" title `mappend`
						constField "body" list `mappend`
						defaultContext)
				>>= loadAndApplyTemplate "template/default.html"
					(constField "title" title `mappend`
						defaultContext)
				>>= relativizeUrls

	-- Add favicon
	match "favicon.png" $ do
		route idRoute
		compile copyFileCompiler

	-- Add raw CSS
	match "css/*.css" $ do
		route   idRoute
		compile compressCssCompiler

	-- Add Clay-based css
	match "css/*.hs" $ do
		route $ setExtension "css"
		compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

	-- Add some default pages
	match (fromList ["about.md", "code.md"]) $ do
		route   $ setExtension "html"
		compile $ pandocCompiler
			>>= loadAndApplyTemplate "template/default.html" defaultContext
			>>= relativizeUrls

	-- Add images
	match "img/*" $ do
		route idRoute
		compile copyFileCompiler

	match "post/*" $ do
		route $ setExtension "html"
		compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
			>>= loadAndApplyTemplate "template/post.html"    (tagsCtx tags)
			>>= loadAndApplyTemplate "template/default.html" (tagsCtx tags)
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
				>>= loadAndApplyTemplate "template/default.html" archiveCtx
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
				>>= loadAndApplyTemplate "template/default.html" (homeCtx tags list)
				>>= relativizeUrls

	match "template/*" $ compile templateCompiler
	where
	pandocOptions :: WriterOptions
	pandocOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathJax "" }

postCtx :: Context String
postCtx =
	dateField "date" "%Y-%m-%d" `mappend`
	defaultContext

archiveCtx :: Context String
archiveCtx =
	constField "title" "Archive" `mappend`
	defaultContext

tagsCtx :: Tags -> Context String
tagsCtx tags = tagsField "prettytags" tags `mappend` postCtx

homeCtx :: Tags -> String -> Context String
homeCtx tags list =
	constField "post" list `mappend`
	constField "title" "Home" `mappend`
	field "taglist" (\_ -> renderTagList tags) `mappend`
	defaultContext

postList
	:: Tags
	-> Pattern
	-> ([Item String] -> Compiler [Item String])
	-> Compiler String
postList tags pattern sortFilter = do
	posts <- sortFilter =<< loadAll pattern
	itemTpl <- loadBody "template/post-item.html"
	applyTemplateList itemTpl (tagsCtx tags) posts
