module Main where

import Parse
import ParseExpr
import ParseType
import ParseStat
import ParseDec

main = getLine >>= (print . runParse parseDec) >> main
