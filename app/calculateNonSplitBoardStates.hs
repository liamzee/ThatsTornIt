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
import Control.Monad (join)
import Data.Set (Set, cartesianProduct)
import qualified Data.Set as Set
import CalculateTwoToAce (twoToAceSet)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Merge (sort)


-- | Creates all board states without a split using a helper function
-- applied to the initial set of two-card player card combinations.

allNonSplitBoardStates :: Set BoardState
allNonSplitBoardStates = cartesianProduct allPlayerHands twoToAceSet 


allPlayerHandsIncludingBust :: Set (Vector Card)
allPlayerHandsIncludingBust = Set.unions $ Set.map appendRemainder playerHandsBase

  where

-- Generates all boardstates as combinations, not permutations,
-- to cut down on the amount of calculation necessary.

-- | All two card player hands with the second card not less than the
-- first case, using filter to get rid of empty elements generated

    playerHandsBase :: Set (Vector Card)
    playerHandsBase =
        Set.filter (not . Prelude.null) $ Set.unions $ Set.map createPlayerBases twoToAceSet

-- | Helper function used to generate hands with the first being greater or equal to
-- the second card.

    createPlayerBases :: Card -> Set (Vector Card)
    createPlayerBases firstElement =
        Set.map createPlayerBasesInner twoToAceSet
      where
        createPlayerBasesInner :: Card -> Vector Card
        createPlayerBasesInner secondElement =
            if firstElement > secondElement
                then Vec.empty
                else firstElement `cons` pure secondElement


-- | Appends remaining possible hands.

    appendRemainder :: Vector Card -> Set (Vector Card)
    appendRemainder playerCards =
        if 6 == Vec.length playerCards || checkIfBust playerCards
            then Set.singleton playerCards
            else Set.singleton playerCards `Set.union` newHands
      where
        newHands :: Set (Vector Card)
        newHands = Set.unions $ Set.map appendRemainder $ Set.map (playerCards `snoc`) filteredNewCards

        filteredNewCards :: Set Card
        filteredNewCards = Set.filter (Vec.last playerCards <=) twoToAceSet


allPlayerHands :: Set (Vector Card)
allPlayerHands =
    Set.unions $ Set.map appendRemainder playerHandsBase

  where

-- Generates all boardstates as combinations, not permutations,
-- to cut down on the amount of calculation necessary.

-- | All two card player hands with the second card not less than the
-- first case, using filter to get rid of empty elements generated

    playerHandsBase :: Set (Vector Card)
    playerHandsBase =
        Set.filter (not . Prelude.null) $ Set.unions $ Set.map createPlayerBases twoToAceSet

-- | Helper function used to generate hands with the first being greater or equal to
-- the second card.

    createPlayerBases :: Card -> Set (Vector Card)
    createPlayerBases firstElement =
        Set.map createPlayerBasesInner twoToAceSet
      where
        createPlayerBasesInner :: Card -> Vector Card
        createPlayerBasesInner secondElement =
            if firstElement > secondElement
                then Vec.empty
                else firstElement `cons` pure secondElement


-- | Appends remaining possible hands.

    appendRemainder :: Vector Card -> Set (Vector Card)
    appendRemainder playerCards =
        if 6 == Vec.length playerCards
            then Set.singleton playerCards
            else Set.singleton playerCards `Set.union` newHands
      where
        newHands :: Set (Vector Card)
        newHands = Set.unions $ Set.map appendRemainder $ Set.map (playerCards `snoc`) filteredNewCards

        filteredNewCards :: Set Card
        filteredNewCards = Set.filter (\u  -> Vec.last playerCards <= u && (not . checkIfBust) (playerCards `snoc` u)) twoToAceSet