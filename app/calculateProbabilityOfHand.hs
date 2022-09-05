{-# LANGUAGE ViewPatterns #-}
module CalculateProbabilityOfHand where

import Data.Vector
import qualified Data.Vector as Vec
import CalculateTypes (Card (..))

-- | Recursive tail-calling function that multiplies an accumulator to
-- multiply an initial chance by the odds of the card vector
-- being drawn in that specific order.

-- Wonky in the sense that, first, you have to specify an initial
-- accumulator, and second, the cards in play have to be
-- produced from a boardstate first.

calculateOddsOf :: Vector Card -> Vector Card -> Double -> Double
calculateOddsOf cardsInPlay (Vec.null -> True) acc = acc
calculateOddsOf cardsInPlay inputVector acc =
    let front = inputVector ! 0
        rear = Vec.tail inputVector in
    calculateOddsOf (cardsInPlay `snoc` front) rear
            (calculateDrawChances cardsInPlay front * acc)

-- | Fairly straightforward odds calculator for the odds of
-- drawing a single card.

calculateDrawChances :: Vector Card -> Card -> Double
calculateDrawChances cardsInPlay card =
    let baseNumberOfCards =
            if card == TenJackQueenKing
               then 128
               else 32 in
    fromIntegral
    (
        baseNumberOfCards -
        Vec.length (Vec.filter (card==) cardsInPlay)
    )
    /
    fromIntegral
    (
        416 - Vec.length cardsInPlay
    )

-- Useful here, as it might be reused by multiple modules.

-- Combines preprocess boardstate for computation by calculateOddsOf.

boardStateToCardsInPlay :: (Vector Card, Card, Vector Card) -> Vector Card
boardStateToCardsInPlay (playerCards, dealerFaceUp, removedCards) =
        playerCards <> removedCards `snoc` dealerFaceUp
