module ParseDec
	(parseDec)
where

import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Char (isAlpha, isDigit)
import Control.Monad (guard)

import Syntax
import Parse
import ParseSpace
import ParseIdent
import ParseType
import ParseStat
import ParseExpr

parseAccessModifier :: Parse Char Access
parseAccessModifier = greedy $ parseEither
	(lits "publ" >> kspace >> return Public)
	(return Private)

parseExtendableAccessModifier :: Parse Char ExtendableAccess
parseExtendableAccessModifier = greedy $ choice
	[lits "publ" >> kspace >> return (NoExtend Public)
	,lits "extd" >> kspace >> return Extend
	,return (NoExtend Private)]

data ComplexAccessModifier = PublicWithExclusions (Set.Set String) | PrivateComplex

parseAccessExclusionOption :: Parse Char String
parseAccessExclusionOption = greedy $ many $ choice [litCond isAlpha, litCond isDigit, lit '_']

parseComplexAccessModifier :: Parse Char ComplexAccessModifier
parseComplexAccessModifier = greedy $ parseEither public (return PrivateComplex) where
	public = do
		lits "publ"
		exclusions <- parseEither parseExclusions (kspace >> return Set.empty)
		optional kspace
		return $ PublicWithExclusions exclusions
	parseExclusions = do
		optional kspace
		lit '('
		optional kspace
		lits "priv"
		kspace
		options <- kcommaSeparated $ parseAccessExclusionOption
		guard $ not $ null options
		optional kspace
		lit ')'
		return $ Set.fromList options

parseRuntimeArgDef :: Parse Char ArgumentDef
parseRuntimeArgDef = greedy $ do
	mode <- parseEither (lit '#' >> optional kspace >> return NamedArg) (return PositionalArg)
	name <- parseLocalIdent
	optional kspace
	lit ':'
	optional kspace
	t <- parseType
	return $ RuntimeArgumentDef mode name t

parseStaticArgDef :: Parse Char ArgumentDef
parseStaticArgDef = greedy $ do
	lits "static"
	kspace
	name <- optional $ do
		lit '#'
		optional kspace
		name <- parseLocalIdent
		optional kspace
		lit ':'
		optional kspace
		return name
	t <- parseType
	return $ StaticArgumentDef name t

parseArgDef :: Parse Char ArgumentDef
parseArgDef = parseEither parseRuntimeArgDef parseStaticArgDef

parseArgDefList :: Parse Char [ArgumentDef]
parseArgDefList = kparenthesized parseArgDef

parseRetType :: Parse Char Type
parseRetType = fmap (fromMaybe VoidType) $ greedy $ optional $ do
		optional kspace
		lits "->"
		optional kspace
		parseType

parseFuncDec :: Parse Char TemplatizableDec
parseFuncDec = greedy $ do
	access <- parseExtendableAccessModifier
	lits "func"
	kspace
	name <- parseUnresolvedIdent
	optional kspace
	args <- parseArgDefList
	retType <- parseRetType
	optional kspace
	body <- parseBody
	return $ FuncDec access name args retType body

parseSpecialArg :: Parse Char SpecialArgument
parseSpecialArg = greedy $ do
	lit '('
	optional kspace
	name <- parseLocalIdent
	optional kspace
	lit ':'
	optional kspace
	t <- parseType
	optional kspace
	lit ')'
	return $ SpecialArgument name t

parseGetterDec :: Parse Char TemplatizableDec
parseGetterDec = greedy $ do
	access <- parseExtendableAccessModifier
	lits "getter"
	optional kspace
	receiver <- parseSpecialArg
	optional kspace
	lit '.'
	optional kspace
	name <- parseUnresolvedIdent
	args <- fmap (fromMaybe []) $ optional (optional kspace >> parseArgDefList)
	optional kspace
	lits "->"
	optional kspace
	t <- parseType
	optional kspace
	body <- parseBody
	return $ GetterDec access name receiver args t body

parseSetterMode :: Parse Char SetterMode
parseSetterMode = greedy $ do
	mode <- parseEither
		(lits "constr" >> return ConstructiveSetter)
		(lits "destr"  >> return DestructiveSetter)
	kspace
	return mode

parseSetterDec :: Parse Char TemplatizableDec
parseSetterDec = greedy $ do
	access <- parseExtendableAccessModifier
	setterMode <- parseSetterMode
	lits "setter"
	optional kspace
	receiver <- parseSpecialArg
	optional kspace
	lit '.'
	optional kspace
	name <- parseUnresolvedIdent
	args <- fmap (fromMaybe []) $ optional (optional kspace >> parseArgDefList)
	optional kspace
	lit '='
	optional kspace
	newValArg <- parseSpecialArg
	optional kspace
	body <- parseBody
	return $ SetterDec access setterMode name receiver args newValArg body

parseMethodDec :: Parse Char TemplatizableDec
parseMethodDec = greedy $ do
	lits "method"
	kspace
	name <- parseUnresolvedIdent
	optional kspace
	lit '('
	optional kspace
	lits "dynamic"
	kspace
	dynArg <- parseArgDef
	mainArgs <- fmap (fromMaybe []) $ optional $ greedy $ do
		optional kspace
		lit ','
		optional kspace
		kcommaSeparated parseArgDef
	optional kspace
	lit ')'
	retType <- parseRetType
	optional kspace
	body <- parseBody
	return $ MethodDec name dynArg mainArgs retType body

parseStructCaseAccess :: Parse Char StructCaseAccess
parseStructCaseAccess = greedy $ do
	rawOptions <- parseComplexAccessModifier
	case rawOptions of
		PublicWithExclusions exclusions -> do
			guard $ Set.fromList ["make"] >= exclusions
			let constructorAccess = if Set.member "make" exclusions
				then PrivateConstructor
				else PublicConstructor
			return $ PublicCase constructorAccess
		PrivateComplex -> return PrivateCase

parseFieldAccess :: Parse Char (GetterAccess, SetterAccess)
parseFieldAccess = greedy $ do
	rawOptions <- parseComplexAccessModifier
	case rawOptions of
		PublicWithExclusions exclusions -> case Set.toList exclusions of
			["getter"] -> return (PrivateGetter, PublicSetter)
			["setter"] -> return (PublicGetter, PrivateSetter)
			[] -> return (PublicGetter, PublicSetter)
			_ -> parseFailure
		PrivateComplex -> return (PrivateGetter, PrivateSetter)

-- This function should probably be refactored into some
-- utility file
partitionWith :: (x -> Either a b) -> [x] -> ([a], [b])
partitionWith _ [] = ([], [])
partitionWith f (x : xs) =
	let (as, bs) = partitionWith f xs
	in case f x of
		Left a  -> (a : as, bs)
		Right b -> (as, b : bs)

parseFieldContent :: Parse Char FieldContent
parseFieldContent = greedy $ parseEither initializer fieldType where
	initializer = do
		lit '='
		optional kspace
		expr <- parseExpr
		return $ FieldInitializer expr
	fieldType = do
		lit ':'
		optional kspace
		t <- parseType
		return $ FieldType t

parseField :: Parse Char Field
parseField = greedy $ do
	(getterAccess, setterAccess) <- parseFieldAccess
	mode <- parseEither (lits "var" >> return VarBinding) (lits "let" >> return LetBinding)
	kspace
	name <- parseLocalIdent
	optional kspace
	content <- parseFieldContent
	ksemicolon
	return $ Field getterAccess setterAccess mode name content

parseStructSubcase :: Parse Char (LocalIdent, StructCase)
parseStructSubcase = greedy $ do
	access <- parseStructCaseAccess
	lits "case"
	kspace
	name <- parseLocalIdent
	optional kspace
	(fields, subCases) <- parseEither
		parseStructCaseBody
		(ksemicolon >> return ([], []))
	return $ (name, StructCase access fields subCases)

parseStructElement :: Parse Char (Either Field (LocalIdent, StructCase))
parseStructElement = parseEither
	(fmap Left parseField)
	(fmap Right parseStructSubcase)

parseStructCaseBody :: Parse Char ([Field], [(LocalIdent, StructCase)])
parseStructCaseBody = greedy $ do
	lit '{'
	elements <- greedyMany $ do
		optional kspace
		parseStructElement
	optional kspace
	lit '}'
	return $ partitionWith id elements

parseStructDec :: Parse Char TemplatizableDec
parseStructDec = greedy $ do
	access <- parseStructCaseAccess
	mode <- parseEither (lits "ref" >> kspace >> return RefStruct) (return ValueStruct)
	lits "struct"
	kspace
	params <- greedy $ choice
		-- marked greedy so that "struct $AB {}" cannot parse as "struct $A B {}"
		[kparenthesized parseTemplateParam
		,fmap (\p -> [p]) parseTemplateParam
		,return []]
	optional kspace
	name <- parseLocalIdent
	optional kspace
	(fields, subCases) <- parseStructCaseBody
	return $ StructDec mode params name $ StructCase access fields subCases

parseFuncReq :: Parse Char ProtocolRequirement
parseFuncReq = greedy $ do
	lits "func"
	kspace
	name <- parseUnresolvedIdent
	optional kspace
	args <- kparenthesized parseArgDefInterface
	retType <- parseRetType
	ksemicolon
	return $ FuncRequirement name args retType

parseOptArgInterfaces :: Parse Char [ArgumentDefInterface]
parseOptArgInterfaces =
	fmap (fromMaybe []) $ optional $ optional kspace >> kparenthesized parseArgDefInterface

parseGetterReq :: Parse Char ProtocolRequirement
parseGetterReq = greedy $ do
	lits "getter"
	kspace
	lit '('
	optional kspace
	receiver <- parseType
	optional kspace
	lit ')'
	optional kspace
	lit '.'
	optional kspace
	name <- parseUnresolvedIdent
	args <- parseOptArgInterfaces
	optional kspace
	lits "->"
	optional kspace
	retType <- parseType
	ksemicolon
	return $ GetterRequirement name receiver args retType

parseSetterReq :: Parse Char ProtocolRequirement
parseSetterReq = greedy $ do
	mode <- optional $ parseSetterMode
	lits "setter"
	kspace
	lit '('
	optional kspace
	receiver <- parseType
	optional kspace
	lit ')'
	optional kspace
	lit '.'
	optional kspace
	name <- parseUnresolvedIdent
	args <- parseOptArgInterfaces
	optional kspace
	lit '='
	optional kspace
	t <- parseType
	ksemicolon
	return $ SetterRequirement mode name receiver args t

parseExternalProtocolReq :: Parse Char ProtocolRequirement
parseExternalProtocolReq = greedy $ do
	lits "protocol"
	kspace
	name <- parseUnresolvedIdent
	optional kspace
	params <- kparenthesized parseType
	ksemicolon
	return $ ExternalProtocolRequirement name params

parseProtocolReq :: Parse Char ProtocolRequirement
parseProtocolReq = greedy $ choice
	[parseFuncReq
	,parseGetterReq
	,parseSetterReq
	,parseExternalProtocolReq]

parseProtocolReqs :: Parse Char [ProtocolRequirement]
parseProtocolReqs = greedy $ do
	lit '{'
	requirements <- many $ optional kspace >> parseProtocolReq
	optional kspace
	lit '}'
	return requirements

parseTemplateParam :: Parse Char LocalIdent
parseTemplateParam = lit '$' >> parseLocalIdent

parseProtocolDec :: Parse Char Dec
parseProtocolDec = greedy $ do
	access <- parseAccessModifier
	lits "protocol"
	kspace
	name <- parseLocalIdent
	optional kspace
	params <- kparenthesized $ lit '$' >> parseLocalIdent
	optional kspace
	requirements <- parseProtocolReqs
	return $ ProtocolDec access name params requirements

parseOpenDec :: Parse Char Dec
parseOpenDec = greedy $ do
	lits "open"
	kspace
	openType <- choice
		[lits "func"   >> return OpenFunc
		,lits "getter" >> return OpenGetter
		,lits "setter" >> return OpenSetter]
	kspace
	name <- parseLocalIdent
	ksemicolon
	return $ OpenDec openType name

parseTemplatizableDec :: Parse Char TemplatizableDec
parseTemplatizableDec = choice
	[parseFuncDec
	,parseStructDec
	,parseGetterDec
	,parseSetterDec
	,parseMethodDec]

parseTemplatizedDec :: Parse Char Dec
parseTemplatizedDec = greedy $ do
	requirements <- fmap (fromMaybe []) $ greedy $ optional $ do
		lits "given"
		optional kspace
		reqs <- parseProtocolReqs
		optional kspace
		return reqs
	dec <- parseTemplatizableDec
	return $ TemplatizedDec requirements dec

parseDec :: Parse Char Dec
parseDec = choice
	[parseTemplatizedDec
	,parseProtocolDec
	,parseOpenDec]
