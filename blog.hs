{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

main :: IO ()
main = hakyll $ do
	{- match "images/*" $ do
		route   idRoute
		compile copyFileCompiler
		-}

	match "favicon.png" $ do
		route idRoute
		compile copyFileCompiler

	match "css/*.css" $ do
		route   idRoute
		compile compressCssCompiler

	match "css/*.hs" $ do
		route $ setExtension "css"
		compile $ getResourceString >>= withItemBody (unixFilter "runghc" [])

	match (fromList ["about.md", "code.md"]) $ do
		route   $ setExtension "html"
		compile $ pandocCompiler
			>>= loadAndApplyTemplate "template/default.html" defaultContext
			>>= relativizeUrls

	match "post/*" $ do
		route $ setExtension "html"
		compile $ pandocCompiler
			>>= loadAndApplyTemplate "template/post.html"    postCtx
			>>= loadAndApplyTemplate "template/default.html" postCtx
			>>= relativizeUrls

	create ["archive.html"] $ do
		route idRoute
		compile $ do
			let
				archiveCtx =
					field "post" (\_ -> postList recentFirst) `mappend`
					constField "title" "Archive"              `mappend`
					defaultContext

			makeItem ""
				>>= loadAndApplyTemplate "template/archive.html" archiveCtx
				>>= loadAndApplyTemplate "template/default.html" archiveCtx
				>>= relativizeUrls


	match "index.html" $ do
		route idRoute
		compile $ do
			let
				indexCtx = field "post" $ \_ -> postList $ fmap (take 3) . recentFirst

			getResourceBody
				>>= applyAsTemplate indexCtx
				>>= loadAndApplyTemplate "template/default.html" postCtx
				>>= relativizeUrls

	match "template/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
	dateField "date" "%B %e, %Y" `mappend`
	defaultContext

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
	posts <- sortFilter =<< loadAll "post/*"
	itemTpl <- loadBody "template/post-item.html"
	list <- applyTemplateList itemTpl postCtx posts
	return list
