module GameButtonSeq where

import qualified Data.Map as M
import Data.Maybe

-- The database stores a list of Name values, because multiple sequences can
-- have the same name, in which case we store just one instance of the sequence,
-- but a list of multiple Name values.
type ButtonSeqDB = M.Map ButtonSeq [Name]
type ButtonSeq = [String]
type Name = String

-- If a button sequence is already given some set of names, add the current
-- given name into that list of names..
register :: (ButtonSeq, Name) -> ButtonSeqDB -> ButtonSeqDB
register (buttonSeq, name) db = case M.lookup buttonSeq db of
  Just names -> M.insert buttonSeq (name:names) db
  Nothing -> M.insert buttonSeq [name] db

-- Input `buttonSeq` is a list of buttons pressed, from oldest to newest, so
-- that the head of the list contains the oldest button.
onButtons :: ButtonSeq -> ButtonSeqDB -> [Name]
onButtons buttonHist db = concatMap extractNames buttonHists
  where
  buttonHists = take (length buttonHist) $ iterate tail buttonHist
  extractNames bHist = case M.lookup bHist db of
    Just names -> names
    Nothing -> []

onButtons2 :: ButtonSeq -> ButtonSeqDB -> [Name]
onButtons2 buttonHist db = concat $ mapMaybe (flip M.lookup db) buttonHists
  where
  buttonHists = take (length buttonHist) $ iterate tail buttonHist
