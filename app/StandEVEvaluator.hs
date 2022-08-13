{-# LANGUAGE BangPatterns, LambdaCase #-}

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
      probabilityTenJackQueenKing, numberOfCardsIn, probabilityOther, safeInitThroughNil)
import DealerHands
    ( dealerHandsNotSix,
      dealerHands21,
      dealerHandsSix)
import CardValueChecker ( cardsToValue, valueCheck )
import Data.List ((\\), isPrefixOf, isSuffixOf)


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
calculateStandEV cardsInPlay@(playerCards, _) = 
    
    let
    naturalEV =
        2.5 - 1.5 * calcDealer cardsInPlay [natural]
    winMinusLossMinusTie lossHands tieHands =
        2 -
        (2 * calcDealer cardsInPlay lossHands) -
        calcDealer cardsInPlay tieHands
    in

    case playerCards of
        [Ace,Ten_Jack_Queen_King] -> naturalEV
        [Ten_Jack_Queen_King,Ace] -> naturalEV
        playerCards | Prelude.length playerCards == 6 ->  winMinusLossMinusTie
            [   
                natural,
                Prelude.filter (valueCheck (cardsToValue playerCards) (<)) dealerHandsSix
            ]
            [
                Prelude.filter (valueCheck (cardsToValue playerCards) (==)) dealerHandsSix
            ]

            
    {- Note some interesting stuff going on.
    
    2 - loss percent - tie percent =
        
    -2dealerHandsSix -2dealerHandsNatural
    -dealerHands21 + dealerHandsNatural
    
    Consequently, we move dealerHandsNatural to the tie section, which simplifies the calculation.

    -}

        playerCards  | cardsToValue playerCards == 21 ->
            winMinusLossMinusTie
            [
                dealerHandsSix
            ]
            [
                dealerHands21,
                natural
            ]

        playerCards ->
            winMinusLossMinusTie
            [
                dealerHandsSix , 
                filter (valueCheck (cardsToValue playerCards) (<)) dealerHandsNotSix
            ]
            [
                filter (valueCheck (cardsToValue playerCards) (==)) dealerHandsNotSix
            ]

-- |
  
calcDealer :: CardsInPlay -> [[DealerCards]] -> Probability
calcDealer ( playerCards , dealerFaceUp ) =
    \case
        [] ->
            0
        (x:xs) ->
            go x + calcDealer (playerCards, dealerFaceUp) xs

  where
    go :: [DealerCards] -> Probability
    go =
        sum .
        fmap (probabilityOfDealerHand (playerCards++dealerFaceUp) . safeTailThroughNil ) .
        Prelude.filter (isPrefixOf dealerFaceUp ) .
        reverse

-- |
  
probabilityOfDealerHand :: [Card] -> DealerCards -> Probability
probabilityOfDealerHand cardsInPlay = \case
    [] ->
        1
    (Ten_Jack_Queen_King:rest) ->
        probabilityTenJackQueenKing cardsInPlay *
        probabilityOfDealerHand (Ten_Jack_Queen_King:cardsInPlay) rest
    (otherCard:rest) -> 
        probabilityOther cardsInPlay otherCard *
        probabilityOfDealerHand (otherCard:cardsInPlay) rest

