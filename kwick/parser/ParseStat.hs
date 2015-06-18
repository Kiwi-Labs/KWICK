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

parseBindMode :: Parse Char BindMode
parseBindMode = choice
	[lits "var" >> return VarBinding
	,lits "let" >> return LetBinding]

semicolon :: Parse Char ()
semicolon = optional space >> lit ';' >> return ()

parseBindStat :: Parse Char Stat
parseBindStat = greedy $ do
	mode <- parseBindMode
	space
	name <- parseLocalIdent
	optional space
	lit '='
	optional space
	rhs <- parseExpr
	semicolon
	return $ BindStat mode name rhs

parseNewBindStat :: Parse Char Stat
parseNewBindStat = greedy $ do
	lits "new"
	space
	name <- parseLocalIdent
	optional space
	lit ':'
	optional space
	t <- parseType
	semicolon
	return $ NewBindStat name t

parseAssignStat :: Parse Char Stat
parseAssignStat = greedy $ do
	lhs <- delimited (optional space >> lit ',' >> optional space) parseExpr
	guard $ not $ null lhs
	optional space
	lit '='
	optional space
	rhs <- delimited (optional space >> lit ',' >> optional space) parseExpr
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
		optional space
		stat <- parseStat
		return stat
	optional space
	lit '}'
	return stats

parseIfStat :: Parse Char Stat
parseIfStat = greedy $ do
	lits "if"
	space
	condition <- parseExpr
	optional space
	thenClause <- parseBody
	elseIfClauses <- many $ greedy $ do
		optional space
		lits "else"
		space
		lits "if"
		space
		elseCondition <- parseExpr
		optional space
		elseBody <- parseBody
		return (elseCondition, elseBody)
	elseClause <- optional $ do
		optional space
		lits "else"
		optional space
		parseBody
	return $ IfStat ((condition, thenClause) : elseIfClauses) elseClause

parseLoopStat :: Parse Char Stat
parseLoopStat = greedy $ do
	lits "loop"
	optional space
	body <- parseBody
	return $ LoopStat body

parseWhileLoopStat :: Parse Char Stat
parseWhileLoopStat = greedy $ do
	lits "while"
	space
	condition <- parseExpr
	optional space
	body <- parseBody
	return $ WhileLoopStat condition body

parseWhileStat :: Parse Char Stat
parseWhileStat = greedy $ do
	lits "while"
	space
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
	space
	vals <- delimited (optional space >> lit ',' >> optional space) parseExpr
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
