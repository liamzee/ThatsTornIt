{-# LANGUAGE ViewPatterns #-}
module CalculateDealerHandMaps where

import CalculateDealerHands (dealerHands)
import Parallelize (parallelizeDealerHandMap)
import qualified Data.Vector as Vec
import Data.Vector
import CalculateTypes (Card (..))
import Data.Map.Lazy

dealerHandMapFaceUp dealerFaceUp numberOfCardsInPlay =
    parallelizeDealerHandMap dealerHands $ standMapComputeForLength dealerFaceUp numberOfCardsInPlay


standMapComputeForLength :: Card -> Int -> Vector Card -> Double
standMapComputeForLength dealerFaceUp numberOfCardsInPlay hand
    | dealerFaceUp /= Vec.head hand = 0
    | otherwise = calculateOddsOfDealerMap (pure $ Vec.head hand) hand 1 numberOfCardsInPlay


calculateOddsOfDealerMap :: Vector Card -> Vector Card -> Double -> Int -> Double
calculateOddsOfDealerMap cardsInPlay (Vec.null -> True) acc numberOfCardsInPlay = acc
calculateOddsOfDealerMap cardsInPlay inputVector acc numberOfCardsInPlay =
    let front = inputVector Vec.! 0
        rear = Vec.tail inputVector in
    calculateOddsOfDealerMap (cardsInPlay `snoc` front) rear
            (calculateDrawChancesDealerMap cardsInPlay front numberOfCardsInPlay * acc) numberOfCardsInPlay


calculateDrawChancesDealerMap :: Vector Card -> Card -> Int -> Double
calculateDrawChancesDealerMap cardsInPlay card numberOfCardsInPlay =
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
        417 - Vec.length cardsInPlay - numberOfCardsInPlay
    )