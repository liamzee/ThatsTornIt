{-# LANGUAGE ViewPatterns #-}
module CalculateProbabilityOfHand where

import Data.Vector
import qualified Data.Vector as Vec
import CalculateTypes (Card (..), BoardState)

-- | Recursive tail-calling function that multiplies an accumulator to
-- multiply an initial chance by the odds of the card vector
-- being drawn in that specific order.

-- Wonky in the sense that, first, you have to specify an initial
-- accumulator, and second, the cards in play have to be
-- produced from a boardstate first.

calculateOddsOf :: Vector Card -> Vector Card -> Double
calculateOddsOf cardsInPlay handToEvaluate =
    go 1 indexInEvaluation
  where
    totalVector :: Vector Card
    totalVector = cardsInPlay <> handToEvaluate

    indexInEvaluation :: Int
    indexInEvaluation = Vec.length cardsInPlay
    
    go :: Double -> Int -> Double
    go acc index
        | index == Vec.length totalVector = acc
        | otherwise = go ((*) acc $! calculateDrawChances index) (index+1)

-- | Fairly straightforward odds calculator for the odds of
-- drawing a single card.

    calculateDrawChances :: Int -> Double
    calculateDrawChances index =
        let card = totalVector ! index
            selectedVector = Vec.take index totalVector
            baseNumberOfCards =
                if card == TenJackQueenKing
                   then 128
                   else 32 in
        fromIntegral
        (
            baseNumberOfCards -
            Vec.length (Vec.filter (card ==) selectedVector)
        )
        /
        fromIntegral
        (
            416 - Vec.length selectedVector
        )

-- Useful here, as it might be reused by multiple modules.

-- Combines preprocess boardstate for computation by calculateOddsOf.

--Note: this is designed for dealer hands, not playerhands.

boardStateToCardsInPlay :: BoardState -> Vector Card
boardStateToCardsInPlay (playerCards, dealerFaceUp) =
        playerCards `snoc` dealerFaceUp
