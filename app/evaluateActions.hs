{-# LANGUAGE TupleSections #-}

module EvaluateActions where

import CalculateTypes
    ( Action(Surrender, Split, DoubleAction, Hit, Stand),
      BoardState,
      EVAction, EV )
import CalculateStand
    ( calculateStand)
import qualified Data.Vector as Vec
import Data.Vector (Vector, snoc, slice)
import CalculateHandValue (checkIfBust)
import CalculateTwoToAce (twoToAce)
import Control.Arrow ((&&&))
import CalculateProbabilityOfHand (calculateOddsOf, boardStateToCardsInPlay)
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Parallelize (parallelizeLazy)
import qualified Data.List
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Merge (sort)
import BoardStatesSeed (seedBoardStates)


--Current probable errors in how double and split are calculated.


--evaluation functions for top-level game conditions.
--Note that evalHitStand will be called by calculateHit.

evaluateHitStand :: BoardState -> EVAction
evaluateHitStand boardState@(playerCards,dealerFaceUp,removedCards) =
    evaluateHitStandMap Map.! boardState
  where
--The memoization of the algorithm run on all valid states.

    evaluateHitStandMap :: Map BoardState EVAction
    evaluateHitStandMap =
        parallelizeLazy allNonSplitBoardStates evaluateHitStandInner

--Note that evaluateHitStandInner effectively calls itself, via evaluateHitStand
--It introduces a base-state explicitly, which helps to limit the number of computations required.

evaluateHitStandInner :: BoardState -> EVAction
evaluateHitStandInner boardState@(playerHands, dealerFaceUp, removedCards)
    | 6 == length playerHands =
        (calculateStand boardState, Stand)
    | otherwise =
        max (calculateHit, Hit) (calculateStand boardState, Stand)
  where

--CalculateHit starts by running appendNewCard, then appends

    calculateHit :: EV
    calculateHit =
        Vec.sum $
        uncurry (*) .
        (
        calculateOddsOfNewCard &&&
        checkForBustCarrier evaluateHitStand
        )
        <$>
        appendNewCard boardState

    
    calculateOddsOfNewCard :: BoardState -> Double
    calculateOddsOfNewCard
        newBoardState@(newPlayerHand, dealerFaceUp, removedCards) =
            calculateOddsOf (boardStateToCardsInPlay boardState)
                (pure $ Vec.last newPlayerHand)



checkForBustCarrier :: (BoardState -> EVAction) -> BoardState -> EV
checkForBustCarrier function boardState@(playerCards,_,_)=
    if checkIfBust playerCards
        then -1
        else fst $ function $ sortPlayerCards boardState


sortPlayerCards :: BoardState -> BoardState
sortPlayerCards (playerCards, dealerFaceUp, removedCards) =
    (
        runST $ do
            mvec <- Vec.thaw playerCards
            sort mvec
            Vec.freeze mvec,
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
            (pure $ Vec.last newPlayerHand)


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
                &&&
                checkForBustCarrier evaluateDouble
        )
        <$>
        appendNewCard (slice 0 1 playerCards, dealerFaceUp, removedCards)
    )
    
  where

    calculateOddsOfNewCard :: BoardState -> Double
    calculateOddsOfNewCard
        newBoardState@(newPlayerHand, _, _) =
            calculateOddsOf (boardStateToCardsInPlay boardState)
                (pure $ Vec.last newPlayerHand)


calculateDouble :: BoardState -> EV
calculateDouble boardState = 
    (2*) $
    Vec.sum $
    uncurry (*) .
    (
        calculateOddsOfNewCard &&&
        checkForBustCarrier ( (,Stand). calculateStand)
    )
    <$>
    appendNewCard boardState

  where
    
    calculateOddsOfNewCard :: BoardState -> Double
    calculateOddsOfNewCard
        newBoardState@(newPlayerHand, _, _) =
            calculateOddsOf (boardStateToCardsInPlay boardState)
                (pure $ Vec.last newPlayerHand)


