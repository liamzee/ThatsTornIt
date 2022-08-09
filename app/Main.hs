module Main where

import TTI
import DataDeclarations

main :: IO ()
main = (print $ evaluateHitVsStand ([Ace, Tens], [Tens])) >> getChar >> pure ()

