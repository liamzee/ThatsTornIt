
module TotalEVTester where

import Data.Vector (Vector, (!), fromList, snoc, empty)
import qualified Data.Vector as Vec
import Data.Bifunctor (bimap)
import EvaluateActions (evaluateSplitDoubleSurrender, evaluateDoubleSurrender, calculateDouble, calculateSplit, surrender, evaluateHitStand)
import CalculateTypes (BoardState, Card (..), EV, EVAction, Action (Split, DoubleAction))
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import CalculateProbabilityOfHand (calculateOddsOf)
import Control.Arrow ((&&&))
import CalculateTwoToAce (twoToAce)

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
applyApplicableEvaluation boardState@(playerCards, dealerFaceUp)=
    fst  $ case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b ->  evaluateSplitDoubleSurrender
        _ -> evaluateDoubleSurrender

  where
    
    evaluateSplitDoubleSurrender :: EVAction
    evaluateSplitDoubleSurrender =
        maximum
        [
            (calculateSplit boardState, Split),
            (calculateDouble boardState, DoubleAction),
            surrender,
            evaluateHitStand boardState
        ]


    evaluateDoubleSurrender :: EVAction
    evaluateDoubleSurrender =
        maximum
        [
            (calculateDouble boardState, DoubleAction),
            surrender,
            evaluateHitStand boardState
        ]


{-}
listOfStartingHands :: Vector BoardState
listOfStartingHands =  Vec.filter ((==2). Vec.length . (\(a,b,c) -> a)) (allNonSplitBoardStates)
--}

probabilityOfStartingHandsSplitter :: BoardState -> Double
probabilityOfStartingHandsSplitter boardState@(playerCards, dealerFaceUp) =
    case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b -> calculateOddsOf empty (snoc playerCards dealerFaceUp)
        _ -> 2 * calculateOddsOf empty (snoc playerCards dealerFaceUp)

listOfStartingHands :: Vector BoardState
listOfStartingHands =  do
    dealerCard <- twoToAce
    playerFirstCard <- twoToAce
    playerSecondCard <- twoToAce
    if playerFirstCard > playerSecondCard
        then Vec.empty
        else pure (playerFirstCard `Vec.cons` pure playerSecondCard, dealerCard)

--}