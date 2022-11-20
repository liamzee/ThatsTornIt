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
--import Graphics.UI.TinyFileDialogs ( saveFileDialog )
import Data.Text ( Text, unpack )
import TotalEVTester (checkEVofGame)
--import Outputter (packedJSON)
import Data.Vector (Vector)
import CalculateTypes (Card)
import qualified Data.Vector as Vec
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import CalculateDealerHands (countedDealerHands)
import qualified Data.Set
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Merge (sort)
import EvaluateSplit (allPossibleRemovedCards, setForSplitEVStands')
import qualified Data.Foldable
import CalculateHandValue (handValueOf)
import EvaluateSplit (setForSplitEVStands)
import qualified Data.Map as Map
import Outputter (packedJSON)
import EvaluateSplit (benchMarkEV)
import Data.Time (getCurrentTime)

--Rebuilt around 3 main memoizations, namely the
--standEV list, the player hands list, and
--the dealer hands list.

main :: IO ()
main = do
--    print TotalEVTester.checkEVofGame
    --tfd <- saveFileDialog "" "" [""] "" <&> (unpack . fromMaybe "")
    print TotalEVTester.checkEVofGame
    print =<< getCurrentTime
    --print $ Data.Set.size setForSplitEVStands
    --print $ Data.Set.size setForSplitEVStands'
    print $ benchMarkEV
    print =<< getCurrentTime
    {-LB.writeFile
        "C:\\Users\\Liam\\Desktop\\welp.json"
        packedJSON
    --}

{-
possibleRemovedCards :: Vector (Vector Card)
possibleRemovedCards = do
  (playerCards, dealerFaceUp) <- Vec.fromList . Data.Set.toList . Data.Set.fromList  . Vec.toList $ (\(a,b) -> (vectorSort a,b)) <$> allNonSplitBoardStates
  (applicableDealerHands, notChecked) <- Vec.filter ((dealerFaceUp ==). Vec.head . fst) countedDealerHands
  pure (playerCards <> applicableDealerHands)


vectorSort :: Ord a => Vector a -> Vector a
vectorSort hand = runST $ do
      mvec <- Vec.thaw hand
      sort mvec
      Vec.freeze mvec


-}