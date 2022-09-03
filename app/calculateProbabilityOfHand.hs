{-# LANGUAGE ViewPatterns #-}
module CalculateProbabilityOfHand where

import Data.Vector
import qualified Data.Vector as Vec
import CalculateTypes (Card (..))


calculateOddsOf :: Vector Card -> Vector Card -> Double -> Double
calculateOddsOf cardsInPlay (Vec.null -> True) acc = acc
calculateOddsOf cardsInPlay inputVector acc =
    let front = inputVector ! 0
        rear = Vec.tail inputVector in
    calculateOddsOf (cardsInPlay `snoc` front) rear
            (calculateDrawChances cardsInPlay front * acc)


calculateDrawChances :: Vector Card -> Card -> Double
calculateDrawChances cardsInPlay card =
    let baseNumberOfCards
            | card == TenJackQueenKing = 128
            | otherwise = 32 in
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