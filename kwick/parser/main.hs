module Main where

import Parse
import ParseExpr
import ParseType
import ParseStat

main = getLine >>= (print . runParse parseStat) >> main
