module Main where

import Parse
import ParseExpr
import ParseType
import ParseStat

--main = getLine >>= (print . runParse parseStat) >> main
main = (return "if x > 10 { // hello! \n ret x; }") >>= (print . runParse parseStat)
