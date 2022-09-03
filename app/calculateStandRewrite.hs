{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module CalculateStandRewrite where

import CalculateTypes (BoardState, EV, Card (..))
import CalculateDealerHands
import qualified Data.Vector as Vec hiding (elem)
import Data.Vector hiding (elem)
import Data.Function ((&))
import CalculateHandValue (handValueOf, checkIfBust)
import CalculateProbabilityOfHand (calculateOddsOf)
import Data.Map.Lazy ((!), Map)
import Parallelize (parallelize)
import CalculateDealerHandMaps (dealerHandMapFaceUp)
import Data.Foldable
import CalculateStand (isNatural)


probabilityUnder :: BoardState -> (Vector Card -> Vector Card -> Bool) -> EV
probabilityUnder boardState@(playerCards, dealerFaceUp, removedCards) givenFilter =
    let numberOfCardsInPlay = Vec.length playerCards + Vec.length removedCards +1 
        baseMap = dealerHandMapFaceUp dealerFaceUp numberOfCardsInPlay in
    Data.Foldable.sum $ baseMap

-- the subsequent lines are filters for remaining dealerhands that 
-- correspond to a player hand situation, producing both
-- winning and losing hands for a given situation.

-- | win as long as there's no dealer natural. 

naturalWinFilter :: Vector Card -> Vector Card -> Bool
naturalWinFilter playerCards item =
    (not . isNatural) item

-- | win as long as there's no dealer natural, and if the dealer's
-- hand doesn't exceed the player's hand while holding a non-bust
-- six card charlie.

sixCardCharlieWinFilter :: Vector Card -> Vector Card -> Bool
sixCardCharlieWinFilter playerCards item
    | isNatural item ||
        6 == Vec.length item &&
        not (checkIfBust item) &&
        handValueOf playerCards <= handValueOf item =
            False
    | otherwise = True

-- | lose if the dealer has a natural, or if the dealer has a six
-- card charlie that doesn't bust and is of higher value than the
-- player's

sixCardCharlieLossFilter :: Vector Card -> Vector Card -> Bool
sixCardCharlieLossFilter playerCards item
    | isNatural item ||
        6 == Vec.length item &&
        not (checkIfBust item) &&
        handValueOf playerCards < handValueOf item =
            True
    | otherwise = False

-- | win if the dealer doesn't have a natural or non-busting six card charlie,
-- or if the dealer has a normal non-busting hand of lower value
-- than the player's

normalWinFilter :: Vector Card -> Vector Card -> Bool
normalWinFilter playerCards item
    | checkIfBust item =
        True
    | 6 == Vec.length item ||
        isNatural item ||
        handValueOf playerCards <= handValueOf item =
            False
    | otherwise =
        True

-- | lose if the dealer has a natural, a non-busting six card charlie,
-- or a non-busting hand of higher value

normalLossFilter :: Vector Card -> Vector Card -> Bool
normalLossFilter playerCards item
    | checkIfBust item =
        False
    | 6 == Vec.length item ||
      isNatural item ||
      handValueOf playerCards < handValueOf item =
        True
    | otherwise =
        False
