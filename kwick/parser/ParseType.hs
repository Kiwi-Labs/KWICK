module ParseType
	(parseType
	,parseArgDefInterface)
where

import Syntax
import Parse
import ParseIdent
import ParseSpace

parseOpaqueType :: Parse Char [Type]
parseOpaqueType = greedy $ do
	name <- parseUnresolvedIdent
	return [OpaqueType name]

parseVoidType :: Parse Char [Type]
parseVoidType = greedy $ do
	lit '('
	optional kspace
	lit ')'
	return [VoidType]

parseTemplateParameterType :: Parse Char [Type]
parseTemplateParameterType = greedy $ do
	lit '$'
	name <- parseLocalIdent
	return [TemplateParameterType name]

parseParenthesizedTypes :: Parse Char [Type]
parseParenthesizedTypes = kparenthesized parseType

parseArgDefInterface :: Parse Char ArgumentDefInterface
parseArgDefInterface = do
	staticMode <- parseEither
		(lits "static" >> kspace >> return StaticArg)
		(return RuntimeArg)
	maybeName <- optional $ do
		lit '#'
		optional kspace
		name <- parseLocalIdent
		optional kspace
		lit ':'
		optional kspace
		return name
	t <- parseType
	return $ ArgumentDefInterface staticMode maybeName t

parseFunctionTypeArgs :: Parse Char [ArgumentDefInterface]
parseFunctionTypeArgs = kparenthesized parseArgDefInterface

parseFunctionType :: Parse Char [Type]
parseFunctionType = greedy $ do
	lits "func"
	optional kspace
	args <- parseFunctionTypeArgs
	optional kspace
	lits "->"
	optional kspace
	ret <- parseType
	return [FunctionType args ret]

parseAtomicType :: Parse Char [Type]
parseAtomicType = choice
	[parseOpaqueType
	,parseVoidType
	,parseTemplateParameterType
	,parseParenthesizedTypes
	,parseFunctionType]

parseTemplateType :: [Type] -> Parse Char [Type]
parseTemplateType types = do
	optional kspace
	name <- parseUnresolvedIdent
	return [TemplateType name types]

parseListType :: [Type] -> Parse Char [Type]
parseListType [t] = greedy $ do
	optional kspace
	lit '['
	optional kspace
	lit ']'
	return [TemplateType (makeUnresolvedIdent "List") [t]]
parseListType _ = parseFailure

parseDictType :: [Type] -> Parse Char [Type]
parseDictType [valType] = greedy $ do
	optional kspace
	lit '['
	optional kspace
	keyType <- parseType
	optional kspace
	lit ']'
	return $ [TemplateType (makeUnresolvedIdent "Dict") [keyType, valType]]
parseDictType _ = parseFailure

unaryKindParser :: (Type -> Type) -> Char -> [Type] -> Parse Char [Type]
unaryKindParser f char [t] = do
	optional kspace
	lit char
	return [f t]
unaryKindParser _ _ _ = parseFailure

parseType :: Parse Char Type
parseType = do
	types <- chainNest parseAtomicType
		[parseTemplateType
		,parseListType
		,parseDictType
		,unaryKindParser ReferenceType '!'
		,unaryKindParser NullableType '?'
		,unaryKindParser PointerType '&']
	case types of
		[t] -> return t
		_ -> parseFailure
