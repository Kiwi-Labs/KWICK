module ParseExpr
	(parseExpr)
where

-- TODO: Possibly change '@' prefix precedence
--       from parsing @x.y as @(x.y) to (@x).y

import Parse
import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard, forever)
import Data.Maybe (fromMaybe)
import qualified Precedence as Precedence

import Syntax
import ParseIdent
import ParseQuoted
import ParseType

parseStringLitExpr :: Parse Char Expr
parseStringLitExpr = greedy $ StringLitExpr <$> parseQuotedString '"'

parseIntLitExpr :: Parse Char Expr
parseIntLitExpr = greedy $ IntLitExpr <$> parseInt

parseRealLitExpr :: Parse Char Expr
parseRealLitExpr = greedy $ RealLitExpr <$> parseFloating

parseParenthesizedExpr :: Parse Char Expr
parseParenthesizedExpr = greedy $ do
	lit '('
	optional space
	expr <- parseExpr
	optional space
	lit ')'
	return expr

parseAtomicExpr :: Parse Char Expr
parseAtomicExpr = choice
	[BindingExpr <$> parseUnresolvedIdent
	,parseIntLitExpr
	,parseRealLitExpr
	,parseStringLitExpr
	,parseParenthesizedExpr]

parseArgument :: Parse Char Argument
parseArgument = greedy $ do
	maybeName <- optional $ do
		lit '#'
		optional space
		name <- parseLocalIdent
		optional space
		lit ':'
		optional space
		return name
	expr <- parseExpr
	return $ Argument maybeName expr

parseArgumentList :: Parse Char [Argument]
parseArgumentList = do
	lit '('
	optional space
	results <- delimited (optional space >> lit ',' >> optional space) parseArgument
	optional space
	lit ')'
	return results

parseAccessorExpr :: Expr -> Parse Char Expr
parseAccessorExpr expr = greedy $ do
	optional space
	lit '.'
	optional space
	fieldName <- parseUnresolvedIdent
	args <- (fromMaybe []) <$> optional parseArgumentList
	return $ AccessorExpr expr fieldName args

parseFunctionCallExpr :: Expr -> Parse Char Expr
parseFunctionCallExpr expr = greedy $ do
	optional space
	args <- parseArgumentList
	return $ CallExpr expr args

parseSuffixExpr :: Parse Char Expr
parseSuffixExpr = chainNest parseAtomicExpr
	[parseAccessorExpr
	,parseFunctionCallExpr]

prefixParse :: (Expr -> Expr) -> Char -> Parse Char Expr
prefixParse f char = greedy $ do
	lit char
	optional space
	expr <- parseCoreExpr
	return $ f expr

parseNegationPrefix :: Parse Char Expr
parseNegationPrefix =
	prefixParse (\expr ->
		CallExpr (BindingExpr $ UnresolvedIdent ["-"]) [Argument Nothing expr]) '-'

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
operatorIdent op = UnresolvedIdent [name] where
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
		optional space
		op <- parseBinaryOperator
		optional space
		nextTerm <- parseCoreExpr
		return (op, nextTerm)
	let grouped = Precedence.precedenceGroup binaryOperatorGrouping firstTerm restTerms
	return $ precedenceExprToKiwiExpr grouped

precedenceExprToKiwiExpr :: Precedence.Expr Expr BinaryOperator -> Expr
precedenceExprToKiwiExpr (Precedence.Terminal expr) = expr
precedenceExprToKiwiExpr (Precedence.Binop a op b) = let
	a' = precedenceExprToKiwiExpr a
	b' = precedenceExprToKiwiExpr b
	in CallExpr (BindingExpr $ operatorIdent op) [Argument Nothing a', Argument Nothing b']

parsePossibleCastExpr :: Parse Char Expr
parsePossibleCastExpr = do
	expr <- parseBinopSequence
	maybeCastType <- optional $ do
		optional space
		lit ':'
		optional space
		parseType
	return $ case maybeCastType of
		Just castType -> CastExpr expr castType
		Nothing -> expr

parseExpr = parsePossibleCastExpr
