module Main where

import TTI
import DataDeclarations

main :: IO ()
main = (print $ evaluateInitial ([Ace, Tens], [Tens])) >> getChar >> pure ()

