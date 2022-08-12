module Main where

import Data.Time ( getCurrentTime )
import Outputter ( outputMain )


main :: IO ()
main = do
    getCurrentTime >>= print
    outputMain
    getCurrentTime >>= print
