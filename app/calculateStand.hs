{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module CalculateStand where

import CalculateTypes (BoardState, EV, Card (..))
import CalculateDealerHands
import qualified Data.Vector as Vec hiding (elem)
import Data.Vector hiding (elem)
import Data.Function ((&))
import CalculateHandValue (handValueOf, checkIfBust)
import CalculateProbabilityOfHand (calculateOddsOf)
import Data.Map.Lazy ((!), Map, mapWithKey)
import Parallelize (parallelize)
import qualified Data.Foldable
import CalculateDealerHandMaps (dealerHandMapFaceUp)


-- Given a set of player cards, a dealer face up, and removed cards from
-- deck, the player stands. Then, one of three things happen.
-- First, the player wins if:
-- --The player has a natural, and the dealer does not
-- --The player has a six-card charlie, the dealer does not have
--   a natural or six-card charlie
-- --The player has a six-card charlie, the dealer has a six-card charlie
--   but the dealer six-card charlie is of lower value.
-- --The dealer has neither a six-card charlie nor a natural,
--   and the player has a higher hand value.
-- --The player has a normal hand, and the dealer has neither a six-card
--   charlie nor a natural, and the dealer has a hand of lower value.
-- --The dealer busts.
--
-- Second, the player ties if:
-- --The player has a natural, and the dealer has a natural.
-- --The player has a six-card charlie, and the dealer has a six-card charlie
-- -- of the same value.
-- --The player does not have a six-card charlie, the dealer does not have
-- --a six-card charlie, and player-total is equal to dealer total.
--
-- Third, the player loses if:
-- --The dealer has a natural, and the player doesn't.
-- --The dealer has a six-card charlie, and the player has neither a six-
--   card charlie nor a natural.
-- --The dealer has a higher card value than the player without
--   a six-card charlie or natural, the player doesn't have a six-card charlie
--   or natural.
-- --The dealer has a six-card charlie, and the player has a six-card charlie,
--   but the player's hand is of lower value.

-- If we elect to norm to 0 EV (tie is 0 EV, win is 1 EV / 1.5 EV, loss is -1 EV)
-- We can ignore all tie cases.

-- In such a case, we simply need to know, first, what the EV of a win is,
-- second, what dealer hands result in the specific outcome, given the player
-- hand, and third, what the probability of these dealer hands are,
-- given all the cards in play.

-- The formula then roughly comes out to:
-- EV of Win * Probability of Win + EV of Loss (-1) * Probability of Loss


isNatural :: Vector Card -> Bool
isNatural = (`elem` fromList [[Ace, TenJackQueenKing], [TenJackQueenKing, Ace]])


-- The following code assumes that preFilterForDealerFaceUp has already triggered.

probabilityOfEvent :: BoardState -> Vector (Vector Card) -> Double
probabilityOfEvent boardState listOfHands =
    Vec.sum $ 
    calculateOddsOfDealerHand
        (preProcessBoardStateForOddsCalculation boardState) <$>
        listOfHands

-- Calls a general-purpose odds calculator for the odds of a player hand.
-- This one preprocesses because preFilterForDealerFaceUp assigns the first card
-- a probability of one, and all other first cards a probability of zero.

calculateOddsOfDealerHand :: Vector Card -> Vector Card -> Double
calculateOddsOfDealerHand cardsInPlay assessedHand =
    calculateOddsOf cardsInPlay (Vec.drop 1 assessedHand) 1

-- Combines preprocess boardstate for computation by calculateOddsOf.

preProcessBoardStateForOddsCalculation :: (Vector Card, Card, Vector Card) -> Vector Card
preProcessBoardStateForOddsCalculation
    (playerCards, dealerFaceUp, removedCards) =
        playerCards <> removedCards `snoc` dealerFaceUp 

-- Filters away dealerHands that don't have the correct initial card.

preFilterForDealerFaceUp :: Card -> Vector (Vector Card)
preFilterForDealerFaceUp dealerFaceUp =
    Vec.filter ((==dealerFaceUp). Vec.head) dealerHands

-- The core function of this module.

calculateStand :: BoardState -> EV
calculateStand n = mapStandEV Data.Map.Lazy.! n


mapStandEV :: Map BoardState EV
mapStandEV = parallelize calculateStandInner


calculateStandInner :: BoardState -> EV
calculateStandInner boardState@(playerCards, _, _) =
    eVWin playerCards *
    winProbability boardState
    +
    (-1 *
    lossProbability boardState
    )

-- If natural, then the EV return on win is 1.5.

eVWin :: Vector Card -> Double
eVWin playerCards
    | playerCards & isNatural =
        1.5
    | otherwise = 1

-- Basically a splitter function that feeds the boardState to a probabilityOfEvent calculator.

winProbability :: BoardState -> Double
winProbability boardState@(playerCards, dealerFaceUp, _)
    | playerCards & isNatural =
        probabilityUnder boardState naturalWinFilter
    | 6 == Vec.length playerCards =
        probabilityUnder boardState sixCardCharlieWinFilter
    | otherwise =
        probabilityUnder boardState normalWinFilter

-- Basically a splitter function that feeds the boardState to a probabilityOfEvent calculator.

lossProbability :: BoardState -> Double
lossProbability boardState@(playerCards, dealerFaceUp, _)
    | playerCards & isNatural =
        0
    | 6 == Vec.length playerCards =
        probabilityUnder boardState sixCardCharlieLossFilter
    | otherwise =
        probabilityUnder boardState normalLossFilter

-- a function to remove repeated use of filters in hands
{-
probabilityUnder :: BoardState -> (Vector Card -> Vector Card -> Bool) -> EV
probabilityUnder boardState@(playerCards, dealerFaceUp, _) givenFilter =
    probabilityOfEvent boardState $ Vec.filter (givenFilter playerCards) $
        preFilterForDealerFaceUp dealerFaceUp
-}
{- experimental, for performance evaluation -}


probabilityUnder :: BoardState -> (Vector Card -> Vector Card -> Bool) -> EV
probabilityUnder boardState@(playerCards, dealerFaceUp, removedCards) givenFilter =
    let numberOfCardsInPlay = Vec.length playerCards + Vec.length removedCards +1 
        baseMap = dealerHandMapFaceUp dealerFaceUp numberOfCardsInPlay in
    Data.Foldable.sum $ recalculateOdds boardState $ applyPreFilter (givenFilter playerCards) baseMap

applyPreFilter :: (Vector Card -> Bool) -> Map (Vector Card) EV -> Map (Vector Card) EV
applyPreFilter givenFilter baseMap =
    mapWithKey preFilterInner baseMap
  where
    preFilterInner _ 0 = 0
    preFilterInner cards eV
        | givenFilter cards = eV
        | otherwise = 0

recalculateOdds boardState@(playerCards, _, removedCards) baseMap =
    mapWithKey recalculateOddsInner baseMap
  where
    cardsInPlayWithoutDealerFaceUp =
        playerCards <> removedCards

    recalculateOddsInner _ 0 = 0
    recalculateOddsInner cards eV
        | Data.Foldable.any id $
            (`elem` cardsInPlayWithoutDealerFaceUp) <$> cards =
                recalculate cards
        | otherwise = eV

    recalculate cards =
        calculateOddsOfDealerHand
        (preProcessBoardStateForOddsCalculation boardState) cards


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
