{-# LANGUAGE TupleSections #-}

module EvaluateActions where

import CalculateTypes
    ( Action(Surrender, Split, DoubleAction, Hit, Stand),
      BoardState,
      EVAction, EV, Probability, Card )
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
import EvaluateSplit (firstSplit)


--Current probable errors in how double and split are calculated.


--evaluation functions for top-level game conditions.
--Note that evalHitStand will be called by calculateHit.

evaluateHitStand :: BoardState -> EVAction
evaluateHitStand =
    (parallelizeLazy allNonSplitBoardStates evaluateHitStandInner !)

--Note that evaluateHitStandInner effectively calls itself, via evaluateHitStand
--It introduces a base-state explicitly, which helps to limit the number of computations required.

evaluateHitStandInner :: BoardState -> EVAction
evaluateHitStandInner boardState@(playerHands, dealerFaceUp)
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

    
    calculateOddsOfNewCard :: BoardState -> Probability
    calculateOddsOfNewCard
        (newPlayerHand, dealerFaceUp) =
            calculateOddsOf (boardStateToCardsInPlay boardState)
                (pure $ Vec.last newPlayerHand)



checkForBustCarrier :: (BoardState -> EVAction) -> BoardState -> EV
checkForBustCarrier function boardState@(playerCards,dealerFaceUp)=
    if checkIfBust playerCards
        then -1
        else fst $ function $ sortPlayerCards boardState


sortPlayerCards :: BoardState -> BoardState
sortPlayerCards (playerCards, dealerFaceUp) =
    (
        runST $ do
            mvec <- Vec.thaw playerCards
            sort mvec
            Vec.freeze mvec,
        dealerFaceUp
    )


appendNewCard :: BoardState -> Vector BoardState
appendNewCard boardState@(playerCards, dealerFaceUp) =
    (,dealerFaceUp) . snoc playerCards <$> twoToAce


calculateOddsOfNewCard :: BoardState -> BoardState -> Probability
calculateOddsOfNewCard
    oldBoardState@(playerHand, dealerFaceUp)
    newBoardState@(newPlayerHand, otherDealerFaceUp) =
        calculateOddsOf (boardStateToCardsInPlay oldBoardState)
            (pure $ Vec.last newPlayerHand)


evaluateSplitDoubleSurrender :: Map (Card, Card, Vector Card) EV -> BoardState -> EVAction
evaluateSplitDoubleSurrender mapping boardState@(playerCards, dealerFaceUp) =
    maximum
        [
            calculateSplit mapping boardState,
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
calculateSplit :: Map (Card, Card, Vector Card) EV -> BoardState -> EVAction
calculateSplit mapping boardState@(playerCards, dealerFaceUp) = 
    firstSplit (Vec.head playerCards) dealerFaceUp mapping

{-}    Vec.sum 
    (
        uncurry (*) .
        (
            calculateOddsOfNewCard
                &&&
                checkForBustCarrier evaluateDoubleSurrender
        )
        <$>
        appendNewCard (slice 0 1 playerCards, dealerFaceUp)
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
        appendNewCard (slice 0 1 playerCards, dealerFaceUp)
    )
    
  where

    calculateOddsOfNewCard :: BoardState -> Probability
    calculateOddsOfNewCard
        newBoardState@(newPlayerHand, dealerFaceUp) =
            calculateOddsOf (boardStateToCardsInPlay boardState)
                (pure $ Vec.last newPlayerHand)
--}

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
    
    calculateOddsOfNewCard :: BoardState -> Probability
    calculateOddsOfNewCard
        newBoardState@(newPlayerHand, dealerFaceUp) =
            calculateOddsOf (boardStateToCardsInPlay boardState)
                (pure $ Vec.last newPlayerHand)


