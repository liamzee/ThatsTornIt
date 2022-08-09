module Main where

import TTI
import DataDeclarations
import Data.Time

main :: IO ()
main = getCurrentTime >>= print >> (print $ evaluateInitial ([Two, Two], [Two])) >> getCurrentTime >>= print >> pure ()

