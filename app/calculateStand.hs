{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module CalculateStand where

import CalculateTypes (BoardState, EV, Card (..), Probability, PlayerCards)
import CalculateDealerHands
import qualified Data.Vector as Vec hiding (elem)
import Data.Vector hiding (elem)
import CalculateHandValue (handValueOf, checkIfBust, checkForSoft17)
import CalculateProbabilityOfHand
    ( calculateOddsOf, boardStateToCardsInPlay )
import Data.Map.Lazy ((!), Map)
import Parallelize (parallelize)
import qualified Data.Set as Set
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import qualified Data.Vector.Mutable as Vm
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Merge (sort)
import Control.Arrow ((&&&),(***))
import CalculateTwoToAce (twoToAce)
import Control.Applicative (liftA2)
import qualified Data.Map
import qualified Data.List


-- Given a set of player cards, a dealer face up, and removed cards from
-- deck, the player stands. Then, one of three things happen.
-- First, the player wins if:
-- --The player has a natural, and the dealer does not
-- --The player has a six-card-charlie, the dealer does not have
--   a natural or six-card-charlie
-- --The player has a six-card-charlie, the dealer has a six-card-charlie
--   of lower value.
-- --The dealer has neither a six-card charlie nor a natural,
--   and the player has a higher hand value.
-- --The dealer busts.
--
-- Second, the player ties if:
-- --The player has a natural, and the dealer has a natural.
-- --The player has a six-card charlie, and the dealer has a six-card charlie
-- -- of the same value.
-- --The player does not have a six-card charlie, the dealer does not have
-- --a six-card charlie, and both player and dealer have equal hand value.
--
-- Third, the player loses if:
-- --The dealer has a natural, and the player doesn't.
-- --The dealer has a six-card charlie, and the player has neither a six-
--   card charlie nor a natural.
-- --The dealer has a six-card charlie, and the player has a six-card charlie,
--   but the player's hand is of lower value.
-- --Neither the dealer nor the player have either a six-card charlie or a
--   natural, and the player has a hand of lower value.

-- If we elect to norm to 0 EV (tie is 0 EV, win is 1 EV / 1.5 EV, loss is -1 EV)
-- We can ignore all tie cases on the top-level calculation.

-- In such a case, we simply need to know, first, what the EV of a win is,
-- second, what dealer hands result in the specific outcome, given the player
-- hand, and third, what the probability of these dealer hands are,
-- given all the cards in play.

-- The formula then roughly comes out to:
-- EV of Win * Probability of Win + EV of Loss (-1) * Probability of Loss

-- Probability of win = 1 - probability of loss - probability of tie.
-- This allows us to compute only loss and tie cases, allowing us to avoid
-- the generation and calculation of dealer bust cases.

-- | a check to see if a hand is a natural.

isNatural :: Vector Card -> Bool
isNatural = (`elem` fromList [[Ace, TenJackQueenKing], [TenJackQueenKing, Ace]])


-- | The following code assumes that preFilterForDealerFaceUp has already triggered,
-- and compresses the Vector Card , Int tuple into a single figure to sum up.

probabilityOfEvent :: BoardState -> Vector (Vector Card, Int) -> Probability
probabilityOfEvent boardState listOfHands =
    Vec.sum $
    uncurry (*) .
    (calculateOddsOfDealerHand boardState *** fromIntegral) <$>
        listOfHands

-- Calls a general-purpose odds calculator for the odds of a player hand.
-- This one preprocesses because preFilterForDealerFaceUp assigns the first card
-- a probability of one, and all other first cards a probability of zero.

calculateOddsOfDealerHand :: BoardState -> Vector Card -> Probability
calculateOddsOfDealerHand boardState assessedHand =
    calculateOddsOf (boardStateToCardsInPlay boardState)
        (Vec.tail assessedHand)

-- Filters away dealerHands that don't have the correct initial card.

preFilterForDealerFaceUp :: Card -> Vector (Vector Card, Int)
preFilterForDealerFaceUp dealerFaceUp =
    Vec.filter ((==dealerFaceUp) . Vec.head . fst) countedDealerHands

-- The core function of this module.

calculateStand :: BoardState -> EV
calculateStand = (parallelize allNonSplitBoardStates calculateStandInner Data.Map.Lazy.!)
  where

    calculateStandInner :: BoardState -> EV
    calculateStandInner boardState@(playerCards, dealerFaceUp) =

            eVWin
        *   winProbability
        +
            evLoss
        *   lossProbability

      where

        winProbability :: Probability
        winProbability =
            1 - tieProbability - lossProbability
    
        evLoss :: EV
        evLoss = -1
    
-- If natural, then the EV return on win is 1.5.

        eVWin :: EV
        eVWin 
            | isNatural playerCards =
                1.5
            | otherwise = 1

-- A splitter function to evaluate player cases.

        tieProbability :: Probability
        tieProbability
            | isNatural playerCards =
                probabilityUnder naturalTieFilter
            | 6 == Vec.length playerCards =
                probabilityUnder sixCardCharlieTieFilter
            | otherwise =
                probabilityUnder normalTieFilter

-- Basically a splitter function that feeds the boardState to a probabilityOfEvent calculator.

        lossProbability :: Probability
        lossProbability
            | isNatural playerCards =
                0
            | 6 == Vec.length playerCards =
                probabilityUnder sixCardCharlieLossFilter
            | otherwise =
                probabilityUnder normalLossFilter

-- a function to remove repeated use of filters in win / loss probability splitter.

        probabilityUnder :: (Vector Card -> Bool) -> Probability
        probabilityUnder givenFilter =
            probabilityOfEvent boardState $ Vec.filter (givenFilter . fst) $
                preFilterForDealerFaceUp dealerFaceUp

-- the subsequent lines are filters for remaining dealerhands that 
-- correspond to a player hand situation, producing both
-- winning and losing hands for a given situation.

-- | tie if there's a dealer natural.

        naturalTieFilter :: Vector Card -> Bool
        naturalTieFilter =
            isNatural

-- | tie if the opponent has a six card charlie of equal value.

        sixCardCharlieTieFilter :: Vector Card -> Bool
        sixCardCharlieTieFilter item =
            6 == Vec.length item &&
                handValueOf playerCards == handValueOf item

-- | lose if the dealer has a natural, or if the dealer has a six
-- card charlie that doesn't bust and is of higher value than the
-- player's

        sixCardCharlieLossFilter :: Vector Card -> Bool
        sixCardCharlieLossFilter item =
            isNatural item ||
                (6 == Vec.length item &&
            handValueOf playerCards < handValueOf item)

-- | tie if the opponent has a non-six card charlie hand,
-- non-natural hand of equal value.

        normalTieFilter :: Vector Card -> Bool
        normalTieFilter item =
            not (isNatural item) &&
                6 /= Vec.length item &&
                handValueOf playerCards == handValueOf item

-- | lose if the dealer has a natural, a non-busting six card charlie,
-- or a non-busting hand of higher value

        normalLossFilter :: Vector Card -> Bool
        normalLossFilter item =
            6 == Vec.length item ||
                isNatural item ||
                handValueOf playerCards < handValueOf item