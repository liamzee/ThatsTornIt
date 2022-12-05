{-# LANGUAGE ApplicativeDo #-}

module CalculateDealerHands where

import CalculateTwoToAce (twoToAce)
import CalculateTypes (Card (..))
import Data.Vector
    ( Vector, snoc,
     empty )
import qualified Data.Vector as Vec
import Control.Applicative (Applicative(liftA2))
import CalculateHandValue (checkIfBust, checkForSoft17, handValueOf)
import Control.Arrow ((&&&))
import Data.Vector.Algorithms.Merge (sort)
import Control.Monad.ST (runST)
import qualified Data.Vector.Mutable as Vm
import Data.Set (Set)
import qualified Data.Set as Set


countedDealerHandsIncludingBust :: Vector (Vector Card, Int)
countedDealerHandsIncludingBust =
    makeNumber . vectorSort $ internalSort <$> dealerHands
  where
-- | The collection of dealer hands, excluding bust dealer hands.
-- Since we are not computing hands against which the player wins,
-- using tie and loss to substitute for that, we don't have to calculate
-- for that anymore, simplifying and reducing computational load.

    dealerHands :: Vector (Vector Card)
    dealerHands =
        dealerHandsCore >>= appendToCore

-- | All initial two-carded dealer hands, as permutations.

    dealerHandsCore :: Vector (Vector Card)
    dealerHandsCore =
        liftA2 (snoc . pure) twoToAce twoToAce

-- | Used to transform dealerHandsCore recursively into all
-- accessible, non-bust dealer hands.

    appendToCore :: Vector Card -> Vector (Vector Card)
    appendToCore hand

-- If the hand is already length 6, we return it, if it's soft 17, we return it.
-- This simulates the dealer strategy in our format, i.e, stand on soft 17.
-- Otherwise, we try to append new cards, checking if bust (and returning empty
-- in that case), using monadic bind to compress back to vectors.

        | 6 == Vec.length hand || checkForSoft17 hand =
            pure hand
        | otherwise = twoToAce >>= appendToCore . snoc hand

-- | Direct mutable vector sort, not really that much faster than
-- transforming the target into a list.

    vectorSort :: Vector (Vector Card) -> Vector (Vector Card)
    vectorSort hand = runST $ do
        mvec <- Vec.thaw hand
        sort mvec
        Vec.freeze mvec

-- | Sorts every card after the first of a vector. This is needed
-- because of the face-up shenanigans we're using.

    internalSort :: Vector Card -> Vector Card
    internalSort hand = runST $ do
        mvec <- Vec.thaw hand
        sort $ Vm.slice 1 (Vec.length hand - 1) mvec
        Vec.freeze mvec

-- | The actual function that makes numbers, using group to group
-- the pre-sorted vectors, fanout to create the vector and its length.
-- Will cause problems if you have empty groups.

--simplified and more readable version, courtesy FP discord.
    makeNumber :: Vector (Vector Card) -> Vector (Vector Card, Int)
    makeNumber = Vec.fromList . fmap (Vec.head &&& Vec.length) . Vec.group


countedDealerHands :: Vector (Vector Card, Int)
countedDealerHands =
    makeNumber . vectorSort $ internalSort <$> dealerHands
  where

-- | The collection of dealer hands, excluding bust dealer hands.
-- Since we are not computing hands against which the player wins,
-- using tie and loss to substitute for that, we don't have to calculate
-- for that anymore, simplifying and reducing computational load.

    dealerHands :: Vector (Vector Card)
    dealerHands =
        dealerHandsCore >>= appendToCore

-- | All initial two-carded dealer hands, as permutations.

    dealerHandsCore :: Vector (Vector Card)
    dealerHandsCore =
        liftA2 (snoc . pure) twoToAce twoToAce

-- | Used to transform dealerHandsCore recursively into all
-- accessible, non-bust dealer hands.

    appendToCore :: Vector Card -> Vector (Vector Card)
    appendToCore hand

-- If the hand is already length 6, we return it, if it's soft 17, we return it.
-- This simulates the dealer strategy in our format, i.e, stand on soft 17.
-- Otherwise, we try to append new cards, checking if bust (and returning empty
-- in that case), using monadic bind to compress back to vectors.

        | 6 == Vec.length hand || checkForSoft17 hand =
            pure hand
        | otherwise = do
            newCard <- twoToAce
            let newHand = hand `snoc` newCard
            if checkIfBust newHand
                then empty
                else appendToCore newHand

-- | Direct mutable vector sort, not really that much faster than
-- transforming the target into a list.

    vectorSort :: Vector (Vector Card) -> Vector (Vector Card)
    vectorSort hand = runST $ do
        mvec <- Vec.unsafeThaw hand
        sort mvec
        Vec.unsafeFreeze mvec

-- | Sorts every card after the first of a vector. This is needed
-- because of the face-up shenanigans we're using.

    internalSort :: Vector Card -> Vector Card
    internalSort hand = runST $ do
        mvec <- Vec.thaw hand
        sort $ Vm.slice 1 (Vec.length hand - 1) mvec
        Vec.freeze mvec

-- | The actual function that makes numbers, using group to group
-- the pre-sorted vectors, fanout to create the vector and its length.
-- Will cause problems if you have empty groups.

--simplified and more readable version, courtesy FP discord.
    makeNumber :: Vector (Vector Card) -> Vector (Vector Card, Int)
    makeNumber = Vec.fromList . fmap (Vec.head &&& Vec.length) . Vec.group
