module Parser.ParseSpace
	(kspace
	,kcommaSeparated
	,kparenthesized
	,ksingleOrParenthesized
	,ksemicolon)
where

import Parser.Parse
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

kparenthesized :: Parse Char a -> Parse Char [a]
kparenthesized p = greedy $ do
	lit '('
	optional kspace
	content <- kcommaSeparated p
	optional kspace
	lit ')'
	return content

ksingleOrParenthesized :: Parse Char a -> Parse Char [a]
ksingleOrParenthesized p = parseEither (fmap (\x -> [x]) p) (kparenthesized p)

ksemicolon :: Parse Char ()
ksemicolon = optional kspace >> lit ';' >> return ()
