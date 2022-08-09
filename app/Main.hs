module Main where

import DataDeclarations
import Data.Time
import TopLevelEvaluator

main :: IO ()
main = do
    
    getCurrentTime >>= print
    (print $ evaluateInitial ([Ace, Ace], [Nine]))
    getCurrentTime >>= print
    pure ()

