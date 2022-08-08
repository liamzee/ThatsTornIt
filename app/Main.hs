module Main where

import TTI

main :: IO ()
main = (print $ evaluateHitVsStand ([Two, Two], [Tens])) >> getChar >> pure ()
