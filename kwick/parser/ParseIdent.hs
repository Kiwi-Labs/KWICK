module ParseIdent
	(parseUnresolvedIdent
	,parseLocalIdent)
where

import qualified Data.Set as Set
import Data.Char (isAlpha, isAlphaNum)
import Control.Monad (guard)
import Control.Applicative ((<$>), (<*>))

import Syntax (UnresolvedIdent (..), LocalIdent (..))
import Parse
import ParseQuoted

reservedWords :: Set.Set String
reservedWords = Set.fromList
	["func"
	,"publ"
	,"priv"
	,"var"
	,"let"
	,"new"
	,"if"
	,"else"
	,"extract"
	,"for"
	,"in"
	,"while"
	,"loop"
	,"break"
	,"ret"
	,"continue"
	,"val"
	,"struct"
	,"case"
	,"class"
	,"method"
	,"static"
	,"dynamic"
	,"ref"
	,"getter"
	,"setter"
	,"constr"
	,"destr"
	,"reffer"
	,"protocol"
	,"given"
	,"import"]

isLocalIdentStartChar :: Char -> Bool
isLocalIdentStartChar '_' = True
isLocalIdentStartChar c = isAlpha c

isLocalIdentChar :: Char -> Bool
isLocalIdentChar '_' = True
isLocalIdentChar c = isAlphaNum c

parseNormalLocalIdent :: Parse Char LocalIdent
parseNormalLocalIdent = greedy $ do
	name <- (:) <$> (litCond isLocalIdentStartChar) <*> (many $ litCond isLocalIdentChar)
	guard $ Set.notMember name reservedWords
	return $ LocalIdent name

parseQuotedLocalIdent :: Parse Char LocalIdent
parseQuotedLocalIdent = greedy $ LocalIdent <$> parseQuotedString '\''

parseLocalIdent :: Parse Char LocalIdent
parseLocalIdent = parseEither parseNormalLocalIdent parseQuotedLocalIdent

parseUnresolvedIdent :: Parse Char UnresolvedIdent
parseUnresolvedIdent = greedy $ do
	names <- delimited1 (lits "::") parseLocalIdent
	let path = map (\(LocalIdent name) -> name) names
	return $ UnresolvedIdent path
