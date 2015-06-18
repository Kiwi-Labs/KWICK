module ParseQuoted
	(parseQuotedString)
where

import Parse
import Control.Monad (guard)
import qualified Data.Map as Map

escapes :: Map.Map Char Char
escapes = Map.fromList
	[('\\', '\\')
	,('"', '"')
	,('\'', '\'')
	,('n', '\n')
	,('r', '\r')
	,('t', '\t')
	,('a', '\a')]

parseQuotedString :: Char -> Parse Char String
parseQuotedString q = greedy $ do
	let escapedChar = do
		lit '\\'
		char <- consume1
		case Map.lookup char escapes of
			Just escape -> return escape
			Nothing -> parseFailure
	
	let normalChar = do
		char <- consume1
		guard $ char /= q && char /= '\\'
		return char
	
	lit q
	content <- reluctantMany $ parseEither normalChar escapedChar
	lit q
	return content
