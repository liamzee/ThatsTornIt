{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module CalculateStand where

import CalculateTypes (BoardState, EV, Card (..))
import CalculateDealerHands
import qualified Data.Sequence as Sequ
import Data.Sequence (Seq((:|>), Empty, (:<|)), fromList, index, drop, filter)
import Data.Function ((&))
import CalculateHandValue (handValueOf, checkIfBust)


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

-- If we elect to norm to 0 EV (tie is 0 EV, win is 1 EV / 1.5 EV, loss is -1 EV)
-- We can ignore all tie cases.

-- In such a case, we simply need to know, first, what the EV of a win is,
-- second, what dealer hands result in the specific outcome, given the player
-- hand, and third, what the probability of these dealer hands are,
-- given all the cards in play.

-- The formula then roughly comes out to:
-- EV of Win * Probability of Win + EV of Loss (-1) * Probability of Loss

calculateStand :: BoardState -> EV
calculateStand boardState@(playerCards, _, _) =
    eVWin playerCards *
    winProbability boardState
    +
    (-1 *
    lossProbability boardState
    )


eVWin :: Seq Card -> Double
eVWin playerCards
    | playerCards & isNatural =
        1.5
    | otherwise = 1


isNatural :: Seq Card -> Bool
isNatural = flip elem (fromList [[Ace, TenJackQueenKing], [TenJackQueenKing, Ace]])


probabilityOfEvent :: BoardState -> Seq (Seq Card) -> Double
probabilityOfEvent boardState listOfHands =
    sum $ fmap (calculateOddsOf (preProcessBoardStateForOddsCalculation boardState)) listOfHands


preProcessBoardStateForOddsCalculation :: (Seq b, b, Seq b) -> (Seq b, b)
preProcessBoardStateForOddsCalculation
    (playerCards, dealerFaceUp, removedCards) =
        ( playerCards <> removedCards :|> dealerFaceUp , dealerFaceUp)


preFilterForDealerFaceUp :: Card -> Seq (Seq Card)
preFilterForDealerFaceUp dealerFaceUp =
    Sequ.filter ((==dealerFaceUp).(`index` 0)) dealerHands


winProbability :: BoardState -> Double
winProbability boardState@(playerCards, dealerFaceUp, _)
    | playerCards & isNatural =
        probabilityOfEvent boardState $ naturalWinFilter boardState $
            preFilterForDealerFaceUp dealerFaceUp
    | 6 == length playerCards =
        probabilityOfEvent boardState $ sixCardCharlieWinFilter boardState $
            preFilterForDealerFaceUp dealerFaceUp
    | otherwise =
        probabilityOfEvent boardState $ normalWinFilter boardState $
            preFilterForDealerFaceUp dealerFaceUp


lossProbability :: BoardState -> Double
lossProbability boardState@(playerCards, dealerFaceUp, _)
    | playerCards & isNatural =
        0
    | 6 == length playerCards =
        probabilityOfEvent boardState $ sixCardCharlieLossFilter boardState $
            preFilterForDealerFaceUp dealerFaceUp
    | otherwise =
        probabilityOfEvent boardState $ normalLossFilter boardState $
            preFilterForDealerFaceUp dealerFaceUp


naturalWinFilter :: BoardState -> Seq (Seq Card) -> Seq (Seq Card)
naturalWinFilter (_, _, _) handList =
    Sequ.filter
        (`notElem`
        ([[Ace, TenJackQueenKing],[TenJackQueenKing, Ace]] :: Seq (Seq Card)))
    handList


sixCardCharlieWinFilter :: BoardState -> Seq (Seq Card) -> Seq (Seq Card)
sixCardCharlieWinFilter (playerCards, _, _) handList =
    Sequ.filter sixCardCharlieWinList handList
  where
    sixCardCharlieWinList :: Seq Card -> Bool
    sixCardCharlieWinList item 
        | item `elem`
            ([[Ace, TenJackQueenKing],[TenJackQueenKing, Ace]]
            :: Seq (Seq Card)) =
                False
        | 6 /= length item ||
            checkIfBust item ||
            handValueOf playerCards > handValueOf item =
                True
        | handValueOf playerCards <= handValueOf item  = False


sixCardCharlieLossFilter :: BoardState -> Seq (Seq Card) -> Seq (Seq Card)
sixCardCharlieLossFilter (playerCards, _, _) handList = 
    Sequ.filter sixCardCharlieLossList handList
  where
    sixCardCharlieLossList :: Seq Card -> Bool
    sixCardCharlieLossList item
        | item `elem`
            ([[Ace, TenJackQueenKing],[TenJackQueenKing, Ace]]
            :: Seq (Seq Card)) =
                True
        | checkIfBust item ||
            6 /= length item =
                False
        | handValueOf playerCards < handValueOf item =
            True
        | handValueOf playerCards >= handValueOf item = False


normalWinFilter :: BoardState -> Seq (Seq Card) -> Seq (Seq Card)
normalWinFilter (playerCards, _, _) handList =
    Sequ.filter normalWinList handList
  where
    normalWinList :: Seq Card -> Bool
    normalWinList item
        | 6 == length item =
            False
        | checkIfBust item =
            True
        | item `elem`
            ([[Ace, TenJackQueenKing],[TenJackQueenKing, Ace]]
            :: Seq (Seq Card)) =
                False
        | handValueOf playerCards > handValueOf item =
            True
        | handValueOf playerCards <= handValueOf item =
            False


normalLossFilter :: BoardState -> Seq (Seq Card) -> Seq (Seq Card)
normalLossFilter (playerCards, _, _) handList =
    Sequ.filter normalLossList handList
  where
    normalLossList :: Seq Card -> Bool
    normalLossList item
        | 6 == length item =
            True
        | item `elem`
            ([[Ace, TenJackQueenKing],[TenJackQueenKing, Ace]]
            :: Seq (Seq Card)) =
                True
        | checkIfBust item =
            False
        | handValueOf playerCards >= handValueOf item =
            False
        | handValueOf playerCards < handValueOf item =
            True


calculateOddsOf :: (Seq Card, Card) -> Seq Card -> Double
calculateOddsOf (cardsInPlay, dealerFaceUp) assessedHand =
    go cardsInPlay (Data.Sequence.drop 1 assessedHand) 1
  where
    go cardsInPlay Empty acc = acc
    go cardsInPlay (front :<| rear) acc =
        go (cardsInPlay :|> front) rear
            (calculateDrawChances cardsInPlay front * acc)


calculateDrawChances :: Seq Card -> Card -> Double
calculateDrawChances cardsInPlay card =
    let cardsOfTypeLeftInDeck
            | card == TenJackQueenKing = 128
            | otherwise = 32 in
    fromIntegral
    (
        cardsOfTypeLeftInDeck -
        length (Data.Sequence.filter (card==) cardsInPlay)
    )
    /
    fromIntegral
    (
        416 - length cardsInPlay
    )
