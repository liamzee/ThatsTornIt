{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Time (getCurrentTime)
import Data.Aeson ( encode )
import TotalEVTester (checkEVofGame)
import CalculateTypes (Card (..), EV)
import Outputter (packedJSON)
import EvaluateSplit (benchMarkEV)
import qualified Data.ByteString as B 
import Flat ( flat )
import Flat.Run (unflat)
import Flat.Instances.Vector ()
import Data.Either (fromLeft, fromRight)
import EvaluateSplit (firstSplit)
import Data.Vector (Vector, fromList)
import Data.Map (Map, lookup, fromSet)
import qualified Data.List
import qualified Data.Vector as Vec
import CalculateNonSplitBoardStates (allPlayerHandsIncludingBust)
import qualified Data.Set as Set
import CalculateDealerHands (countedDealerHandsIncludingBust)
import RemovedCardsGenerator ( secondGameEVMap)
import Control.DeepSeq (force)
import Control.Parallel.Strategies (using)
import Parallelize (splitEvenlySet)
import qualified Data.Set
import Data.Set (Set)
import EvaluateSplit (parallelizeMapping)
import qualified Data.ByteString.Lazy as LB
import SplitOutputter (packedJSONSplits)

--Rebuilt around 3 main memoizations, namely the
--standEV list, the player hands list, and
--the dealer hands list.

main :: IO ()
main = do
    --print TotalEVTester.checkEVofGame
    --tfd <- saveFileDialog "" "" [""] "" <&> (unpack . fromMaybe "")
    --print $ benchMarkEV
    --print $ Data.Set.size setForSplitEVStands'
    print =<< getCurrentTime
    --B.writeFile "flatStore.flat" $ flat secondGameEVMap
    store <- fromRight (error "failed to decompress code") . unflat <$> B.readFile "flatStore.flat"
    --print $ TotalEVTester.checkEVofGame store
    --print =<< getCurrentTime
    LB.writeFile
        "C:\\Users\\Liam\\Desktop\\splitsNamed.json"
        (packedJSONSplits store)
    LB.writeFile
        "C:\\Users\\Liam\\Desktop\\nonSplitsNamed.json"
        (packedJSON store)

tester :: Map (Card, Card, Vector Card) EV -> IO ()
tester mapping = go Two Two
  where
    go Ace Ace = putStrLn "Ace Ace" >> print (firstSplit Ace Ace mapping)
    go a Ace = putStrLn (show a <> show Ace) >> print (firstSplit a Ace mapping) >> go (succ a) Two
    go a b = putStrLn ((show a) <> (show b)) >> print (firstSplit a b mapping) >> go (a) (succ b)