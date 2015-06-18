module ParseType
	(parseType)
where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)

import Syntax
import Parse
import ParseIdent

parseOpaqueType :: Parse Char [Type]
parseOpaqueType = greedy $ do
	name <- parseUnresolvedIdent
	return [OpaqueType name]

parseTemplateParameterType :: Parse Char [Type]
parseTemplateParameterType = greedy $ do
	lit '$'
	name <- parseLocalIdent
	return [TemplateParameterType name]

parseParenthesizedTypes :: Parse Char [Type]
parseParenthesizedTypes = greedy $ do
	lit '('
	optional space
	types <- commaSeparated parseType
	optional space
	lit ')'
	return types

parseFunctionTypeArg :: Parse Char ArgumentDefInterface
parseFunctionTypeArg = do
	maybeName <- optional $ do
		lit '#'
		optional space
		name <- parseLocalIdent
		optional space
		lit ':'
		optional space
		return name
	t <- parseType
	return $ ArgumentDefInterface maybeName t

parseFunctionTypeArgs :: Parse Char [ArgumentDefInterface]
parseFunctionTypeArgs = do
	lit '('
	optional space
	args <- commaSeparated parseFunctionTypeArg
	optional space
	lit ')'
	return args

parseFunctionType :: Parse Char [Type]
parseFunctionType = do
	lits "func"
	optional space
	args <- parseFunctionTypeArgs
	optional space
	lits "->"
	optional space
	rets <- parseParenthesizedTypes
	return [FunctionType args rets]

parseAtomicType :: Parse Char [Type]
parseAtomicType = choice
	[parseOpaqueType
	,parseTemplateParameterType
	,parseParenthesizedTypes
	,parseFunctionType]

parseTemplateType :: [Type] -> Parse Char [Type]
parseTemplateType types = do
	optional space
	name <- parseUnresolvedIdent
	return [TemplateType name types]

unaryKindParser :: (Type -> Type) -> Char -> [Type] -> Parse Char [Type]
unaryKindParser f char [t] = do
	optional space
	lit char
	return [f t]
unaryKindParser _ _ _ = parseFailure

parseType :: Parse Char Type
parseType = do
	types <- chainNest parseAtomicType
		[parseTemplateType
		,unaryKindParser ReferenceType '!'
		,unaryKindParser NullableType '?'
		,unaryKindParser PointerType '&']
	case types of
		[t] -> return t
		_ -> parseFailure
