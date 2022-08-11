{-# LANGUAGE BangPatterns #-}

module StandEVEvaluator where

import DataDeclarations
    ( ExpectedValue,
      Probability,
      DealerCards,
      CardsInPlay,
      Card(Ten_Jack_Queen_King, Ace) )
import CommonNamesAndFunctions
    ( natural,
      safeTailThroughNil,
      probabilityTenJackQueenKing, numberOfCardsIn, seqIsPrefixOf)
import DealerHands
    ( dealerHandsNotSix,
      dealerHands21,
      dealerHandsNatural,
      dealerHandsSix)
import CardValueChecker ( cardsToValue, valueCheck )
import Data.List ((\\), isPrefixOf)
import Data.Sequence
import qualified Data.Sequence as Seq


{- The following functions calculate the stand EV, which is fundamental for
determining the EV of an action.

Ultimately, all actions eventually recourse to what the stand EV is, because if
  you no longer have valid moves, you must stand.-}


{- Refactoring, there's only three cases that matter. First,
  there's the case of 6 card charlie. Then the player wins provided
  that the dealer doesn't have a natural, and that the opponent doesn't
  have a 6 card charlie of their own.
  
  Second, there's the case of a player natural. The player always wins
  unless the opponent has naturals, in which case there's a tie.
  
  Third, there's the case of player 21. Uniquely here, the player loses to a natural,
  and ties to a dealer 21.
  
  Fourth, there's any other case.
  
  There's also the basic calculation of wins, loss, and ties. In the case of loss,
  since we set EV to 1 on tie, we won't calculate anything. In the case of winning,
  we'll set the yield to 2, unless it's a natural, in which case the yield is 2.5.
  
  The general formula then comes out to:
  
  1 * probability of tie + 2 * probability of winning
  probability of winning = 1 - probability of tie - probability of losing
  
  then:
  
  probability of tie + (2 * (1 - probability of tie - probability of losing)
  
  2 + (-2 * probability of tie) - 2 * probability of losing + probability of tie
  
  2 - 2 * probability of losing - probability of tie.
  
  For naturals:
  
  2.5 - 1.5 * probability of tie
  
  -}

calculateStandEV :: CardsInPlay -> ExpectedValue
calculateStandEV !cardsInPlay@(playerCards, [dealerFaceUp]) = 
    
    let
    naturalEV = 2.5 - 1.5 * calcDealer cardsInPlay dealerHandsNatural
    winMinusLossMinusTie lossHands tieHands = 2 - 2 * calcDealer cardsInPlay lossHands
        - calcDealer cardsInPlay tieHands in

    case playerCards of
        [Ace,Ten_Jack_Queen_King] -> naturalEV
        [Ten_Jack_Queen_King,Ace] -> naturalEV
        playerCards | Prelude.length playerCards == 6 ->  winMinusLossMinusTie
            (
                dealerHandsNatural <>
                Seq.filter (valueCheck (cardsToValue playerCards) (<)) dealerHandsSix
            )
            (
                Seq.filter (valueCheck (cardsToValue playerCards) (==)) dealerHandsSix
            )
        playerCards  | cardsToValue playerCards == 21 -> winMinusLossMinusTie
            (
                dealerHandsSix <>
                dealerHandsNatural
            )
            (
                dealerHands21 
            ) + calcDealer cardsInPlay dealerHandsNatural
        playerCards -> winMinusLossMinusTie
            (
                dealerHandsSix <>
                Seq.filter (valueCheck (cardsToValue playerCards) (<)) dealerHandsNotSix
            )
            (
                Seq.filter (valueCheck (cardsToValue playerCards) (==)) dealerHandsNotSix
            )

  
calcDealer :: CardsInPlay -> Seq DealerCards -> Probability
calcDealer ( playerCards , [dealerFaceUp] ) =
    
    (sum $!). fmap (probabilityOfDealerHand playerCards . safeTailThroughNil ) .
    Seq.filter (isPrefixOf [dealerFaceUp] )
  
probabilityOfDealerHand :: [Card] -> DealerCards -> Probability
probabilityOfDealerHand cardsInPlay dealerCards = case dealerCards of
      
    [] -> 1
    Ten_Jack_Queen_King:xs -> probabilityTenJackQueenKing cardsInPlay *
        probabilityOfDealerHand (Ten_Jack_Queen_King:cardsInPlay) xs
    otherCard:xs -> (( 32 - (fromIntegral . Prelude.length . Prelude.filter (==otherCard) $ cardsInPlay) ) /
        (416 - fromIntegral (Prelude.length cardsInPlay) )) *
        probabilityOfDealerHand (otherCard:cardsInPlay) xs

