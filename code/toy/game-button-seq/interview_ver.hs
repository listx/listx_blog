import Data.List

type ButtonSeqDB = [(ButtonSeq, Name)]
type ButtonSeq = [String]
type Name = String

-- Default DB of button sequences.
buttonSeqDB :: ButtonSeqDB
buttonSeqDB =
	[ (["down", "forward", "punch"], "hadoken")
	, (["forward", "punch"], "charger")
	, (["up", "punch"], "uppercut")
	]

register :: ButtonSeq -> Name -> ButtonSeqDB -> ButtonSeqDB
register sequence name db
	| null sequence = db
	| alreadyExists = db
	| otherwise = (sequence, name) : db
	where
	alreadyExists = elem (sequence, name) db

onButtons :: ButtonSeq -> ButtonSeqDB -> [Name]
onButtons buttonHist db = map snd
	. fst
	. foldl step ([], db)
	$ reverse buttonHist
	where
	step acc@(foundSoFar, dbRem) button
		| null remaining = acc
		| otherwise =
			( foundSoFar ++ entriesComplete
			, dbRem'
			)
		where
		remaining = filter ((==button) . last . fst)
			$ filter (not . null . fst) dbRem
		dbRem' = map (\(a, b) -> (init a, b)) remaining
		entriesComplete = filter (null . fst) dbRem'
