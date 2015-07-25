module ParseIdent
	(parseUnresolvedIdent
	,parseLocalIdent)
where

import qualified Data.Set as Set
import Data.Char (isAlpha, isAlphaNum)
import Control.Monad (guard)
import Control.Applicative ((<$>), (<*>))

import Syntax
import Parse
import ParseQuoted

reservedWords :: Set.Set String
reservedWords = Set.fromList
	["func"
	,"publ"
	,"priv"
	,"extd"
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
	,"value"
	,"struct"
	,"case"
	,"interface"
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
	name <- greedy $ (:) <$> (litCond isLocalIdentStartChar) <*> (many $ litCond isLocalIdentChar)
	guard $ Set.notMember name reservedWords
	return $ LocalIdent name

parseQuotedLocalIdent :: Parse Char LocalIdent
parseQuotedLocalIdent = greedy $ LocalIdent <$> parseQuotedString '\''

parseLocalIdent :: Parse Char LocalIdent
parseLocalIdent = parseEither parseNormalLocalIdent parseQuotedLocalIdent

parseNormalUnresolvedIdent :: Parse Char UnresolvedIdent
parseNormalUnresolvedIdent = greedy $ do
	path <- delimited1 (lits "::") parseLocalIdent
	return $ UnresolvedIdent path

parseLocalModuleIdent :: Parse Char UnresolvedIdent
parseLocalModuleIdent = greedy $ do
	access <- parseEither
		(lits "publ" >> return Public)
		(lits "priv" >> return Private)
	lits "::"
	path <- delimited1 (lits "::") parseLocalIdent
	return $ LocalModuleIdent access path

parseUnresolvedIdent = parseEither parseNormalUnresolvedIdent parseLocalModuleIdent
