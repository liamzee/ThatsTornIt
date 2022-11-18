{-# LANGUAGE OverloadedStrings, LambdaCase, PatternSynonyms #-}
{-# LANGUAGE OverloadedLists, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns, TupleSections, ApplicativeDo #-}

{- Currently requires clean-up to move to the new algorithm. -}

{- Split calculation is also hashed, and hence requires fixing. -}

{- 

STANDING AND IMPORTANT QUESTION:

DOES MY ALGORITHM ACCOUNT FOR PLAYER BUST CASES?

-}

module Main where

import Data.Time (getCurrentTime)
import Data.Aeson ( FromJSON, ToJSON, encode )
import qualified Data.ByteString.Lazy as LB
import Graphics.UI.TinyFileDialogs ( saveFileDialog )
import Data.Text ( Text, unpack )
import TotalEVTester (checkEVofGame)
import Outputter (packedJSON)

--Rebuilt around 3 main memoizations, namely the
--standEV list, the player hands list, and
--the dealer hands list.

main :: IO ()
main = do
--    print TotalEVTester.checkEVofGame
    --tfd <- saveFileDialog "" "" [""] "" <&> (unpack . fromMaybe "")
    print TotalEVTester.checkEVofGame
    {--}
    LB.writeFile
        "C:\\Users\\Liam\\Desktop\\welp.json"
        packedJSON
    --}