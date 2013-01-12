module Blog.Custom
	( dateField'
	)
	where

import Hakyll

import Control.Applicative (Alternative(..))
import Control.Monad (msum)
import qualified Data.Map as M
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format
	( formatTime
	, parseTime
	)
import System.Locale
	( TimeLocale(..)
	, defaultTimeLocale
	)
import System.FilePath (takeFileName)

-- Custom date field parser (use `YYYYMMDD-...' instead of the default
-- `YYYY-MM-DD-...'). The only change is in getUTC'.  See
-- Hakyll.Web.Template.Context module for the originals.
dateField'
	:: String
	-> String
	-> Context a
dateField' = dateFieldWith' defaultTimeLocale

dateFieldWith'
	:: TimeLocale
	-> String
	-> String
	-> Context a
dateFieldWith' locale key format = field key $ \i -> do
	time <- getUTC' locale $ itemIdentifier i
	return $ formatTime locale format time


getUTC'
	:: TimeLocale
	-> Identifier
	-> Compiler UTCTime
getUTC' locale id' = do
	metadata <- getMetadata id'
	let
		tryField k fmt = M.lookup k metadata >>= parseTime' fmt
		fn             = takeFileName $ toFilePath id'

	maybe empty return $ msum $
		[tryField "published" fmt | fmt <- formats] ++
		[tryField "date"      fmt | fmt <- formats] ++
		[parseTime' "%Y%m%d" $ take 8 fn] -- the critical difference
	where
	parseTime' = parseTime locale
	formats    =
		[ "%a, %d %b %Y %H:%M:%S UT"
		, "%Y-%m-%dT%H:%M:%SZ"
		, "%Y-%m-%d %H:%M:%S"
		, "%Y-%m-%d"
		, "%B %e, %Y %l:%M %p"
		, "%B %e, %Y"
		]
