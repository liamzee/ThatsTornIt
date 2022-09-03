{-# LANGUAGE TupleSections #-}

module CalculateNonSplitBoardStates where

import CalculateTwoToAce (twoToAce)
import Control.Applicative (Applicative(liftA2))
import CalculateTypes (Card)
import Data.Vector
import qualified Data.Vector as Vec
import CalculateHandValue (checkIfBust)
import Data.Foldable (Foldable(toList))
import Data.Set

playerHandsBase :: Vector (Vector Card, Card, Vector Card)
playerHandsBase =
    flip (liftA2 (,,Vec.empty)) twoToAce $  Vec.filter (not . Vec.null) $ liftA2 createPlayerBases twoToAce twoToAce


createPlayerBases :: Card -> Card -> Vector Card
createPlayerBases firstElement secondElement =
    if firstElement > secondElement
        then Vec.empty
        else pure firstElement `snoc` secondElement


allNonSplitBoardStates :: Vector (Vector Card, Card, Vector Card)
allNonSplitBoardStates =
    appendRemainder =<< playerHandsBase


appendRemainder :: (Vector Card, Card, Vector Card) -> Vector (Vector Card, Card, Vector Card)
appendRemainder boardState@(playerCards, dealerFaceUp, removedCards)
    | checkIfBust playerCards =
        Vec.empty
    | 6 == Vec.length playerCards =
        pure (playerCards, dealerFaceUp, removedCards)
    | otherwise =
        boardState `cons` (go boardState =<< twoToAce)
  where
    go (playerCards, dealerFaceUp, removedCards) newCard =
        let newPlayerHand = playerCards `snoc` newCard in
        if Vec.last playerCards > newCard
            then Vec.empty
            else appendRemainder (newPlayerHand, dealerFaceUp, removedCards)
