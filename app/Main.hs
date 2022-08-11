{-# LANGUAGE OverloadedLists #-}

module Main where

import DataDeclarations
import Data.Time
import TopLevelEvaluator
import Outputter

main :: IO ()
main = do
    
    getCurrentTime >>= print
    print $ evaluateInitial ([Two, Two],[Two])
    getCurrentTime >>= print
    pure ()

{- What we'd run if we ever got the code up to actually be performant.

main = outputMain

-}