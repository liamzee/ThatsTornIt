module Main where

import Data.Time ( getCurrentTime )
import StandEVEvaluator
import DataDeclarations (Card(..))
import HitStandEvaluator
import TopLevelEvaluator
--import Outputter ( outputMain )


main :: IO ()
main = do
    getCurrentTime >>= print
    print $ evaluateInitial ([Two,Two],[Two])
    getCurrentTime >>= print

