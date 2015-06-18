module ParseSpace
	(kspace
	,kcommaSeparated)
where

import Parse
import Data.Char (isSpace)

parseComment :: Parse Char ()
parseComment = greedy $ do
	lits "//"
	many $ litCond (/= '\n')
	parseEither (lit '\n' >> return ()) endOfSequence
	return ()

kspace :: Parse Char ()
kspace = greedy $ do
	many $ parseEither (litCond isSpace >> return ()) parseComment
	return ()

kcommaSeparated :: Parse Char a -> Parse Char [a]
kcommaSeparated = delimited (optional kspace >> lit ',' >> optional kspace)
