{-# LANGUAGE TupleSections, ApplicativeDo #-}

module CalculateNonSplitBoardStates where

import CalculateTwoToAce (twoToAce)
import Control.Applicative (Applicative(liftA2))
import CalculateTypes (Card, BoardState)
import Data.Vector
import qualified Data.Vector as Vec
import CalculateHandValue (checkIfBust)
import Data.Foldable (Foldable(toList))
import qualified Data.Map.Lazy as Map


-- | Creates all board states without a split using a helper function
-- applied to the initial set of two-card player card combinations.

allNonSplitBoardStates :: Vector Card -> Vector BoardState
allNonSplitBoardStates cardsInPlay =
    playerHandsBase >>= appendRemainder

  where

-- Should be modified to generate Set by default, reduce computation somewhat.

-- Generates all boardstates as combinations, not permutations,
-- to cut down on the amount of calculation necessary.

-- | All two card player hands with the second card not less than the
-- first case, using filter to get rid of empty elements generated

    playerHandsBase :: Vector BoardState
    playerHandsBase =
        flip (liftA2 (,,cardsInPlay)) twoToAce $ 
        Vec.filter (not . Vec.null) $
        liftA2 createPlayerBases twoToAce twoToAce

-- | Helper function used to generate hands with the first being greater or equal to
-- the second card.

    createPlayerBases :: Card -> Card -> Vector Card
    createPlayerBases firstElement secondElement =
        if firstElement > secondElement
            then Vec.empty
            else pure firstElement `snoc` secondElement


-- | Appends remaining possible hands.

-- Somehow seems unperformant, as though it can be simplified.

    appendRemainder :: BoardState -> Vector BoardState
    appendRemainder boardState@(playerCards, dealerFaceUp, removedCards)
        | 6 == Vec.length playerCards =
            pure (playerCards, dealerFaceUp, removedCards)
        | otherwise = (playerCards, dealerFaceUp, removedCards) `cons` (go (playerCards, dealerFaceUp, removedCards) =<< twoToAce)
        where
            go :: BoardState -> Card -> Vector BoardState
            go boardState@(playerCards, dealerFaceUp, removedCards) newCard
                | Vec.last playerCards > newCard ||
                    checkIfBust newPlayerHand =
                        Vec.empty
                | otherwise = appendRemainder (newPlayerHand, dealerFaceUp, removedCards)
                    where
                        newPlayerHand = playerCards `snoc` newCard
