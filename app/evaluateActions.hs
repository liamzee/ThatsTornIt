{-# LANGUAGE TupleSections #-}

module EvaluateActions where

import CalculateTypes
    ( Action(Surrender, Split, DoubleAction, Hit, Stand),
      BoardState,
      EVAction, EV )
import CalculateStand
    ( calculateStand )
import qualified Data.Vector as Vec
import Data.Vector (Vector, snoc, slice)
import CalculateHandValue (checkIfBust)
import CalculateTwoToAce (twoToAce)
import Control.Arrow ((&&&))
import CalculateProbabilityOfHand (calculateOddsOf, boardStateToCardsInPlay)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Parallelize (parallelizeLazy)
import qualified Data.List
import CalculateNonSplitBoardStates (allNonSplitBoardStates)


--Current probable errors in how double and split are calculated.


--evaluation functions for top-level game conditions.
--Note that evalHitStand will be called by calculateHit.

evaluateHitStand :: BoardState -> EVAction
evaluateHitStand boardState =
    evaluateHitStandMap Map.! boardState


evaluateHitStandMap :: Map BoardState EVAction
evaluateHitStandMap =
    parallelizeLazy allNonSplitBoardStates evaluateHitStandInner


evaluateHitStandInner :: BoardState -> EVAction
evaluateHitStandInner boardState@(playerHands, _, _)
    | 6 == length playerHands =
        (calculateStand boardState, Stand)
    | otherwise =
        max (calculateHit boardState, Hit) (calculateStand boardState, Stand)


calculateHit :: BoardState -> EV
calculateHit boardState@(playerHand, dealerFaceUp, removedCards) =
    Vec.sum $
    uncurry (*) .
    (
        calculateOddsOfNewCard boardState &&&
        checkForBustCarrier evaluateHitStand
    )
    <$>
    appendNewCard boardState


checkForBustCarrier :: (BoardState -> EVAction) -> BoardState -> EV
checkForBustCarrier function boardState@(playerCards,_,_)=
    if checkIfBust playerCards
        then -1
        else fst $ function $ sortPlayerCards boardState


sortPlayerCards :: BoardState -> BoardState
sortPlayerCards (playerCards, dealerFaceUp, removedCards) =
    (
        Vec.fromList . Data.List.sort . Vec.toList $
            playerCards,
        dealerFaceUp,
        removedCards
    )


appendNewCard :: BoardState -> Vector BoardState
appendNewCard boardState@(playerCards, dealerFaceUp, removedCards) =
    (,dealerFaceUp,removedCards) . snoc playerCards <$> twoToAce


calculateOddsOfNewCard :: BoardState -> BoardState -> Double
calculateOddsOfNewCard
    oldBoardState@(playerHand, dealerFaceUp, removedCards)
    newBoardState@(newPlayerHand, _, _) =
        calculateOddsOf (boardStateToCardsInPlay oldBoardState)
            (pure $ Vec.last newPlayerHand) 1


evaluateSplitDoubleSurrender :: BoardState -> EVAction
evaluateSplitDoubleSurrender boardState =
    maximum
        [
            (calculateSplit boardState, Split),
            (calculateDouble boardState, DoubleAction),
            surrender,
            evaluateHitStand boardState
        ]


evaluateDoubleSurrender :: BoardState -> EVAction
evaluateDoubleSurrender boardState =
    maximum
        [
            (calculateDouble boardState, DoubleAction),
            surrender,
            evaluateHitStand boardState
        ]


evaluateDouble :: BoardState -> EVAction
evaluateDouble boardState =
    max (calculateDouble boardState, DoubleAction) (evaluateHitStand boardState)


surrender :: EVAction
surrender = (-0.50, Surrender)

-- note that this is a shoddy hack, i.e, it estimates the results based on a constant playerstate, instead of calculating precisely.
-- also note that on the second split, it's been confirmed you can't split if you get the same cards again.
calculateSplit :: BoardState -> EV
calculateSplit boardState@(playerCards, dealerFaceUp, removedCards) = 
    Vec.sum 
    (
        uncurry (*) .
        (
            calculateOddsOfNewCard
                (slice 0 1 playerCards, dealerFaceUp, slice 1 1 playerCards)
                &&&
                checkForBustCarrier evaluateDoubleSurrender
        )
        <$>
        appendNewCard (slice 0 1 playerCards, dealerFaceUp, removedCards)
    )
    +
    Vec.sum
    (
        uncurry (*) .
        (
            calculateOddsOfNewCard
                (slice 0 1 playerCards, dealerFaceUp, slice 1 1 playerCards)
                &&&
                checkForBustCarrier evaluateDouble
        )
        <$>
        appendNewCard (slice 0 1 playerCards, dealerFaceUp, removedCards)
    )


calculateDouble :: BoardState -> EV
calculateDouble boardState = 
    (2*) $
    Vec.sum $
    uncurry (*) .
    (
        calculateOddsOfNewCard boardState &&&
        checkForBustCarrier (\u -> (calculateStand u, Stand))
    )
    <$>
    appendNewCard boardState