module ParseExpr
	(parseExpr)
where

-- TODO: Possibly change '@' prefix precedence
--       from parsing @x.y as @(x.y) to (@x).y

import Parse
import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import qualified Precedence as Precedence

import Syntax
import ParseIdent
import ParseQuoted
import ParseType
import ParseSpace
import {-# SOURCE #-} ParseStat

parseStringLitExpr :: Parse Char Expr
parseStringLitExpr = greedy $ StringLitExpr <$> parseQuotedString '"'

parseIntLitExpr :: Parse Char Expr
parseIntLitExpr = greedy $ IntLitExpr <$> parseInt

parseRealLitExpr :: Parse Char Expr
parseRealLitExpr = greedy $ RealLitExpr <$> parseFloating

parseParenthesizedExpr :: Parse Char Expr
parseParenthesizedExpr = greedy $ do
	lit '('
	optional kspace
	expr <- parseExpr
	optional kspace
	lit ')'
	return expr

parseLambdaArgDef :: Parse Char LambdaArgumentDef
parseLambdaArgDef = do
	mode <- parseEither (lit '#' >> optional kspace >> return NamedArg) (return PositionalArg)
	name <- parseLocalIdent
	t <- optional $ do
		optional kspace
		lit ':'
		optional kspace
		parseType
	return $ LambdaArgumentDef mode name t

parseLambdaArgs :: Parse Char [LambdaArgumentDef]
parseLambdaArgs = greedy $ fmap (fromMaybe []) $ optional $ kparenthesized parseLambdaArgDef

parseLongLambdaExpr :: Parse Char Expr
parseLongLambdaExpr = greedy $ do
	lits "func"
	optional kspace
	args <- parseLambdaArgs
	maybeRetType <- optional $ do
		optional kspace
		lits "->"
		optional kspace
		parseType
	optional kspace
	body <- parseBody
	return $ LambdaExpr args maybeRetType body

parseShortLambdaExpr :: Parse Char Expr
parseShortLambdaExpr = greedy $ do
	args <- parseLambdaArgs
	optional kspace
	lits "=>"
	optional kspace
	expr <- parseExpr
	return $ LambdaExpr args Nothing [ReturnStat expr]

parseAtomicExpr :: Parse Char Expr
parseAtomicExpr = choice
	[BindingExpr <$> parseUnresolvedIdent
	,parseIntLitExpr
	,parseRealLitExpr
	,parseStringLitExpr
	,parseParenthesizedExpr
	,parseLongLambdaExpr
	,parseShortLambdaExpr]

parseRuntimeArgument :: Parse Char Argument
parseRuntimeArgument = greedy $ do
	maybeName <- optional $ do
		lit '#'
		optional kspace
		name <- parseLocalIdent
		optional kspace
		lit ':'
		optional kspace
		return name
	expr <- parseExpr
	return $ RuntimeArgument maybeName expr

parseStaticArgument :: Parse Char Argument
parseStaticArgument = greedy $ do
	lits "static"
	kspace
	maybeName <- optional $ do
		lit '#'
		optional kspace
		name <- parseLocalIdent
		optional kspace
		lit ':'
		optional kspace
		return name
	t <- parseType
	return $ StaticArgument maybeName t

parseArgument :: Parse Char Argument
parseArgument = parseEither parseRuntimeArgument parseStaticArgument

parseArgumentList :: Parse Char [Argument]
parseArgumentList = kparenthesized parseArgument

parseAccessorExpr :: Expr -> Parse Char Expr
parseAccessorExpr expr = greedy $ do
	optional kspace
	lit '.'
	optional kspace
	fieldName <- parseUnresolvedIdent
	optional kspace
	args <- (fromMaybe []) <$> optional parseArgumentList
	return $ AccessorExpr expr fieldName args

parseFunctionCallExpr :: Expr -> Parse Char Expr
parseFunctionCallExpr expr = greedy $ do
	optional kspace
	args <- parseArgumentList
	return $ CallExpr expr args

parseArrowCallExpr :: Expr -> Parse Char Expr
parseArrowCallExpr expr = greedy $ do
	optional kspace
	lits "->"
	optional kspace
	funcName <- parseUnresolvedIdent
	optional kspace
	args <- parseArgumentList
	return $ CallExpr (BindingExpr funcName) $ (RuntimeArgument Nothing expr) : args

parseSuffixExpr :: Parse Char Expr
parseSuffixExpr = chainNest parseAtomicExpr
	[parseAccessorExpr
	,parseFunctionCallExpr
	,parseArrowCallExpr]

prefixParse :: (Expr -> Expr) -> Char -> Parse Char Expr
prefixParse f char = greedy $ do
	lit char
	optional kspace
	expr <- parseCoreExpr
	return $ f expr

parseNegationPrefix :: Parse Char Expr
parseNegationPrefix =
	prefixParse (\expr ->
		CallExpr (BindingExpr $ makeUnresolvedIdent "-") [RuntimeArgument Nothing expr]) '-'

parseCoreExpr :: Parse Char Expr
parseCoreExpr = choice
	[parseNegationPrefix
	,prefixParse RefExpr '!'
	,prefixParse AddressOfExpr '&'
	,prefixParse DereferenceExpr '@'
	,parseSuffixExpr]

data BinaryOperator
	= Add
	| Sub
	| Mul
	| Div
	| Pow
	| Mod
	
	| NotEqual
	| Equal
	| GreaterThan
	| LessThan
	| GreaterThanEqual
	| LessThanEqual
	
	| BitAnd
	| BitOr
	| BitXor
	| BitShiftLeft
	| BitShiftRight
	
	| LogicAnd
	| LogicOr
	
	| Concat
	
	| Range
	deriving (Show)

-- In order of *decreasing* precedence
data BinaryOperatorPrecedenceGroup
	= PowerPrecedence
	| MultiplicativePrecedence
	| AdditivePrecedence
	| CompositionPrecedence
	| ComparisonPrecedence
	| LogicAndPrecedence
	| LogicOrPrecedence
	deriving (Show, Eq, Ord)

precedence :: BinaryOperator -> BinaryOperatorPrecedenceGroup
precedence op = case op of
	Add -> AdditivePrecedence
	Sub -> AdditivePrecedence
	Mul -> MultiplicativePrecedence
	Div -> MultiplicativePrecedence
	Pow -> PowerPrecedence
	Mod -> MultiplicativePrecedence
	
	NotEqual -> ComparisonPrecedence
	Equal -> ComparisonPrecedence
	GreaterThan -> ComparisonPrecedence
	LessThan -> ComparisonPrecedence
	GreaterThanEqual -> ComparisonPrecedence
	LessThanEqual -> ComparisonPrecedence
	
	BitAnd -> AdditivePrecedence
	BitOr -> AdditivePrecedence
	BitXor -> AdditivePrecedence
	BitShiftLeft -> MultiplicativePrecedence
	BitShiftRight -> MultiplicativePrecedence
	
	LogicAnd -> LogicAndPrecedence
	LogicOr -> LogicOrPrecedence
	
	Concat -> CompositionPrecedence
	Range -> CompositionPrecedence

associativity :: BinaryOperatorPrecedenceGroup -> Precedence.Grouping
associativity group = case group of
	PowerPrecedence -> Precedence.GroupRight
	MultiplicativePrecedence -> Precedence.GroupLeft
	AdditivePrecedence -> Precedence.GroupLeft
	CompositionPrecedence -> Precedence.GroupRight
	ComparisonPrecedence -> Precedence.GroupLeft
	LogicAndPrecedence -> Precedence.GroupRight
	LogicOrPrecedence -> Precedence.GroupRight

binaryOperatorGrouping :: BinaryOperator -> BinaryOperator -> Precedence.Grouping
binaryOperatorGrouping op1 op2 = let
	p1 = precedence op1
	p2 = precedence op2
	in case compare p1 p2 of
		LT -> Precedence.GroupLeft
		GT -> Precedence.GroupRight
		EQ -> associativity p1

parseBinaryOperator :: Parse Char BinaryOperator
parseBinaryOperator = choice $ map (\(src, enum) -> lits src >> return enum)
	[("+", Add)
	,("-", Sub)
	,("*", Mul)
	,("/", Div)
	,("**", Pow)
	,("%", Mod)
	
	,("~=", NotEqual)
	,("≠", NotEqual)
	,("==", Equal)
	,(">", GreaterThan)
	,("<", LessThan)
	,(">=", GreaterThanEqual)
	,("<=", LessThanEqual)
	,("≥", GreaterThanEqual)
	,("≤", LessThanEqual)
	
	,("&&", LogicAnd)
	,("||", LogicOr)
	
	,("&", BitAnd)
	,("|", BitOr)
	,("^", BitXor)
	,("<<", BitShiftLeft)
	,(">>", BitShiftRight)
	
	,("...", Range)
	,("..", Concat)]

operatorIdent :: BinaryOperator -> UnresolvedIdent
operatorIdent op = makeUnresolvedIdent name where
	name = case op of
		Add -> "+"
		Sub -> "-"
		Mul -> "*"
		Div -> "/"
		Pow -> "**"
		Mod -> "%"
		
		NotEqual -> "~="
		Equal -> "=="
		GreaterThan -> ">"
		LessThan -> "<"
		GreaterThanEqual -> ">="
		LessThanEqual -> "<="
		
		BitAnd -> "&"
		BitOr -> "|"
		BitXor -> "^"
		BitShiftLeft -> "<<"
		BitShiftRight -> ">>"
		
		LogicAnd -> "&&"
		LogicOr -> "||"
		
		Concat -> ".."
		
		Range -> "..."

parseBinopSequence :: Parse Char Expr
parseBinopSequence = do
	firstTerm <- parseCoreExpr
	restTerms <- many $ do
		optional kspace
		op <- parseBinaryOperator
		optional kspace
		nextTerm <- parseCoreExpr
		return (op, nextTerm)
	let grouped = Precedence.precedenceGroup binaryOperatorGrouping firstTerm restTerms
	return $ precedenceExprToKiwiExpr grouped

precedenceExprToKiwiExpr :: Precedence.Expr Expr BinaryOperator -> Expr
precedenceExprToKiwiExpr (Precedence.Terminal expr) = expr
precedenceExprToKiwiExpr (Precedence.Binop a op b) = let
	a' = precedenceExprToKiwiExpr a
	b' = precedenceExprToKiwiExpr b
	in CallExpr
		(BindingExpr $ operatorIdent op)
		[RuntimeArgument Nothing a', RuntimeArgument Nothing b']

parsePossibleCastExpr :: Parse Char Expr
parsePossibleCastExpr = do
	expr <- parseBinopSequence
	maybeCastType <- optional $ do
		optional kspace
		lit ':'
		optional kspace
		parseType
	return $ case maybeCastType of
		Just castType -> CastExpr expr castType
		Nothing -> expr

parseStatExpr :: Parse Char Expr
parseStatExpr = StatExpr <$> parseCompoundStat

parseExpr = parseEither parsePossibleCastExpr parseStatExpr
