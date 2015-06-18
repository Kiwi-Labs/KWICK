-- This file exists to resolve the mutual dependency between ParseExpr and ParseStat
-- It is conceptually equivalent to a C .h file, or C++ .hpp file.

module ParseStat
	(parseStat
	,parseBody
	,parseCompoundStat)
where

import Syntax
import Parse

parseStat :: Parse Char Stat
parseBody :: Parse Char [Stat]
parseCompoundStat :: Parse Char Stat
