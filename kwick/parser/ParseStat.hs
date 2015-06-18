module ParseStat
	(parseStat
	,parseBody)
where

import Control.Monad (guard)

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

parseBody :: Parse Char [Stat]
parseBody = greedy $ do
	lit '{'
	stats <- many $ greedy $ do
		optional kspace
		stat <- parseStat
		return stat
	optional kspace
	lit '}'
	return stats

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

parseStat = choice
	[parseBindStat
	,parseNewBindStat
	,parseAssignStat
	,parseCallStat
	,parseIfStat
	,parseLoopStat
	,parseWhileLoopStat
	,parseWhileStat
	,parseBreakStat
	,parseContinueStat
	,parseReturnStat]
