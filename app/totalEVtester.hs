
module TotalEVTester where

import Data.Vector (Vector, (!), fromList)
import Data.Vector as Vec
import Data.Bifunctor (bimap)
import EvaluateActions (evaluateSplitDoubleSurrender, evaluateDoubleSurrender)
import CalculateTypes (BoardState, Card (..), EV)
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import CalculateProbabilityOfHand (calculateOddsOf)
import Control.Arrow ((&&&))

--Going to peek ahead and compute EV, just to set up a system of tests.
--Currently, this test is failing substantially and is revealing an EV higher
--than it should be.

checkEVofGame :: Double
checkEVofGame =
    Vec.sum $
    uncurry (*) .
    (probabilityOfStartingHandsSplitter &&& applyApplicableEvaluation)
    <$>
    listOfStartingHands


applyApplicableEvaluation :: BoardState -> EV
applyApplicableEvaluation boardState@(playerCards, dealerFaceUp, removedCards)=
    fst  $ case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b ->  evaluateSplitDoubleSurrender boardState
        _ -> evaluateDoubleSurrender boardState


listOfStartingHands :: Vector BoardState
listOfStartingHands =  Vec.filter ((==2). Vec.length . (\(a,b,c) -> a)) allNonSplitBoardStates


probabilityOfStartingHandsSplitter :: BoardState -> Double
probabilityOfStartingHandsSplitter boardState@(playerCards, dealerFaceUp, removedCards) =
    case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b -> calculateOddsOf empty (snoc playerCards dealerFaceUp) 1
        _ -> 2 * calculateOddsOf empty (snoc playerCards dealerFaceUp) 1
