module Parser.ParseStat
	(parseStat
	,parseBody
	,parseCompoundStat)
where

import Data.Maybe (fromMaybe)

import Parser.Syntax
import Parser.Parse
import Parser.ParseExpr
import Parser.ParseIdent
import Parser.ParseType
import Parser.ParseSpace

parseBindMode :: Parse Char BindMode
parseBindMode = choice
	[lits "var" >> return VarBinding
	,lits "let" >> return LetBinding]

parseBindStat :: Parse Char Stat
parseBindStat = greedy $ do
	mode <- parseBindMode
	kspace
	name <- parseLocalIdent
	optional kspace
	lit '='
	optional kspace
	rhs <- parseExpr
	ksemicolon
	return $ BindStat mode name rhs

parseNewBindStat :: Parse Char Stat
parseNewBindStat = greedy $ do
	lits "new"
	kspace
	name <- parseLocalIdent
	optional kspace
	lit ':'
	optional kspace
	t <- parseType
	ksemicolon
	return $ NewBindStat name t

parseAssignStat :: Parse Char Stat
parseAssignStat = greedy $ do
	lhs <- parseExpr
	optional kspace
	lit '='
	optional kspace
	rhs <- parseExpr
	ksemicolon
	return $ AssignStat lhs rhs

parseCallStat :: Parse Char Stat
parseCallStat = greedy $ do
	expr <- parseExpr
	ksemicolon
	case expr of
		CallExpr receiver args -> return $ CallStat receiver args
		_ -> parseFailure

ordinaryUpdateAssignOperatorParsers :: [Parse Char Expr]
ordinaryUpdateAssignOperatorParsers =
	map (\op -> lits op >> return (BindingExpr $ makeUnresolvedIdent op))
		["+", "-", "*", "/", "%", "**"
		,"&", "|", "^", ">>", "<<"
		,"&&", "||"
		,".."]

parseNamedUpdateAssignOperator :: Parse Char Expr
parseNamedUpdateAssignOperator = greedy $ do
	lit '`'
	name <- parseUnresolvedIdent
	lit '`'
	return $ BindingExpr name

parseUpdateAssignOperator :: Parse Char Expr
parseUpdateAssignOperator =
	greedy $ choice $ ordinaryUpdateAssignOperatorParsers ++ [parseNamedUpdateAssignOperator]

parseUpdateAssignStat :: Parse Char Stat
parseUpdateAssignStat = greedy $ do
	lhs <- parseExpr
	optional kspace
	op <- parseUpdateAssignOperator
	lit '='
	optional kspace
	rhs <- parseExpr
	ksemicolon
	return $ UpdateAssignStat lhs op rhs

parseStatList :: Parse Char [Stat]
parseStatList = greedy $ many $ greedy $ do
		optional kspace
		stat <- parseStat
		return stat

parseBody :: Parse Char [Stat]
parseBody = greedy $ parseEither curlyBody doBody where
	curlyBody = do
		lit '{'
		stats <- parseStatList
		optional kspace
		lit '}'
		return stats
	doBody = do
		lits "do"
		kspace
		parseStatList

parseBlockStat :: Parse Char Stat
parseBlockStat = fmap BlockStat parseBody

parseIfStat :: Parse Char Stat
parseIfStat = greedy $ do
	lits "if"
	kspace
	condition <- parseExpr
	optional kspace
	thenClause <- parseBody
	elseIfClauses <- many $ greedy $ do
		optional kspace
		lits "else"
		kspace
		lits "if"
		kspace
		elseCondition <- parseExpr
		optional kspace
		elseBody <- parseBody
		return (elseCondition, elseBody)
	elseClause <- optional $ do
		optional kspace
		lits "else"
		optional kspace
		parseBody
	return $ IfStat ((condition, thenClause) : elseIfClauses) elseClause

parseLoopStat :: Parse Char Stat
parseLoopStat = greedy $ do
	lits "loop"
	optional kspace
	body <- parseBody
	return $ LoopStat body

parseWhileLoopStat :: Parse Char Stat
parseWhileLoopStat = greedy $ do
	lits "while"
	kspace
	condition <- parseExpr
	optional kspace
	body <- parseBody
	return $ WhileLoopStat condition body

parseForLoopStat :: Parse Char Stat
parseForLoopStat = greedy $ do
	lits "for"
	kspace
	loopVar <- parseLocalIdent
	kspace
	lits "in"
	kspace
	iterator <- parseExpr
	optional kspace
	body <- parseBody
	return $ ForLoopStat loopVar iterator body

parseExtractClause :: Parse Char ExtractClause
parseExtractClause = greedy $ do
	lits "extract"
	kspace
	bindMode <- fmap (fromMaybe LetBinding) $ optional $ do
		lits "var"
		kspace
		return VarBinding
	name <- parseLocalIdent
	possibleExpr <- optional $ do
		optional kspace
		lit '='
		optional kspace
		parseExpr
	(possibleExpr', target) <- case possibleExpr of
		Nothing -> do
			possibleTarget <- optional $ do
				optional kspace
				lit ':'
				optional kspace
				parseType
			let realTarget = case possibleTarget of
				Nothing -> ExtractFromNullable
				Just t -> ExtractToType t
			return (Nothing, realTarget)
		Just (CastExpr expr t) ->
			return (Just expr, ExtractToType t)
		Just expr -> 
			return (Just expr, ExtractFromNullable)
	optional kspace
	body <- parseBody
	return $ ExtractClause bindMode name possibleExpr' target body

parseExtractStat :: Parse Char Stat
parseExtractStat = greedy $ do
	clauses <- delimited1 (optional kspace >> lits "else" >> kspace) parseExtractClause
	return $ ExtractStat clauses

parseWhileStat :: Parse Char Stat
parseWhileStat = greedy $ do
	lits "while"
	kspace
	condition <- parseExpr
	ksemicolon
	return $ WhileStat condition

parseBreakStat :: Parse Char Stat
parseBreakStat = greedy $ do
	lits "break"
	ksemicolon
	return BreakStat

parseContinueStat :: Parse Char Stat
parseContinueStat = greedy $ do
	lits "continue"
	ksemicolon
	return ContinueStat

parseReturnStat :: Parse Char Stat
parseReturnStat = greedy $ do
	lits "ret"
	kspace
	val <- parseExpr
	ksemicolon
	return $ ReturnStat val

parseValueStat :: Parse Char Stat
parseValueStat = greedy $ do
	lits "val"
	kspace
	val <- parseExpr
	ksemicolon
	return $ ValueStat val

parseStat = choice
	[parseBindStat
	,parseNewBindStat
	,parseAssignStat
	,parseCallStat
	,parseUpdateAssignStat
	,parseCompoundStat
	,parseWhileStat
	,parseBreakStat
	,parseContinueStat
	,parseReturnStat
	,parseValueStat]

parseCompoundStat = choice
	[parseBlockStat
	,parseIfStat
	,parseLoopStat
	,parseWhileLoopStat
	,parseForLoopStat
	,parseExtractStat]
