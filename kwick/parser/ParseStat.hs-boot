-- This file exists to resolve the mutual dependency between ParseExpr and ParseStat
-- It is conceptually equivalent to a C .h file, or C++ .hpp file.

module Parser.ParseStat
	(parseStat
	,parseBody
	,parseCompoundStat)
where

import Parser.Syntax
import Parser.Parse

parseStat :: Parse Char Stat
parseBody :: Parse Char [Stat]
parseCompoundStat :: Parse Char Stat
