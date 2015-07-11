module ParseStat
	(parseStat
	,parseBody
	,parseCompoundStat)
where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Syntax
import Parse
import ParseExpr
import ParseIdent
import ParseType
import ParseSpace

parseBindMode :: Parse Char BindMode
parseBindMode = choice
	[lits "var" >> return VarBinding
	,lits "let" >> return LetBinding]

semicolon :: Parse Char ()
semicolon = optional kspace >> lit ';' >> return ()

parseBindStat :: Parse Char Stat
parseBindStat = greedy $ do
	mode <- parseBindMode
	kspace
	name <- parseLocalIdent
	optional kspace
	lit '='
	optional kspace
	rhs <- parseExpr
	semicolon
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
	semicolon
	return $ NewBindStat name t

parseAssignStat :: Parse Char Stat
parseAssignStat = greedy $ do
	lhs <- kcommaSeparated parseExpr
	guard $ not $ null lhs
	optional kspace
	lit '='
	optional kspace
	rhs <- kcommaSeparated parseExpr
	guard $ not $ null rhs
	semicolon
	return $ AssignStat lhs rhs

parseCallStat :: Parse Char Stat
parseCallStat = greedy $ do
	expr <- parseExpr
	semicolon
	case expr of
		CallExpr receiver args -> return $ CallStat receiver args
		_ -> parseFailure

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
	semicolon
	return $ WhileStat condition

parseBreakStat :: Parse Char Stat
parseBreakStat = greedy $ do
	lits "break"
	semicolon
	return BreakStat

parseContinueStat :: Parse Char Stat
parseContinueStat = greedy $ do
	lits "continue"
	semicolon
	return ContinueStat

parseReturnStat :: Parse Char Stat
parseReturnStat = greedy $ do
	lits "ret"
	kspace
	vals <- kcommaSeparated parseExpr
	semicolon
	return $ ReturnStat vals

parseValueStat :: Parse Char Stat
parseValueStat = greedy $ do
	lits "val"
	kspace
	vals <- kcommaSeparated parseExpr
	semicolon
	return $ ValueStat vals

parseStat = choice
	[parseBindStat
	,parseNewBindStat
	,parseAssignStat
	,parseCallStat
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
