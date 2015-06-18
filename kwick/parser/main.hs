module Main where

import Parse
import ParseExpr
import ParseType
import ParseStat

--main = getLine >>= (print . runParse parseStat) >> main
main = (return "x = if x > 0 { val x; } else { val -x; };") >>= (print . runParse parseStat)
