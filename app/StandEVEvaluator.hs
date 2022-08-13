{-# LANGUAGE BangPatterns, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

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
      probabilityTenJackQueenKing, numberOfCardsIn, probabilityOther, safeInitThroughNil, naturalSet)
import DealerHands
    ( dealerHandsNotSix,
      dealerHands21,
      dealerHandsSix, dealerHandsSetSix, dealerHandsSetNotSix21, dealerHandsSetNotSix)
import CardValueChecker ( cardsToValue, valueCheck, valueCheckSet, valueCheckSetVector )
import Data.List ((\\), isPrefixOf, isSuffixOf)
import Data.Set
import qualified Data.Set as Set
import Data.Vector
import qualified Data.Vector as Vec


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
        2.5 - 1.5 * calcDealerSet cardsInPlay [naturalSet]
    winMinusLossMinusTie lossHands tieHands =
        2 -
        (2 * calcDealerSet cardsInPlay lossHands) -
        calcDealerSet cardsInPlay tieHands
    in

    case playerCards of
        [Ace,Ten_Jack_Queen_King] -> naturalEV
        [Ten_Jack_Queen_King,Ace] -> naturalEV
        playerCards | Prelude.length playerCards == 6 ->  winMinusLossMinusTie
            [
                naturalSet,
                Set.filter (valueCheckSetVector (cardsToValue playerCards) (<)) dealerHandsSetSix
            ]
            [
                Set.filter (valueCheckSetVector (cardsToValue playerCards) (==)) dealerHandsSetSix
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
                dealerHandsSetSix
            ]
            [
                dealerHandsSetNotSix21,
                naturalSet
            ]

        playerCards ->
            winMinusLossMinusTie
            [
                dealerHandsSetSix , 
                Set.filter (valueCheckSetVector (cardsToValue playerCards) (<)) dealerHandsSetNotSix
            ]
            [
                Set.filter (valueCheckSetVector (cardsToValue playerCards) (==)) dealerHandsSetNotSix
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
        Prelude.sum .
        fmap (probabilityOfDealerHand (playerCards <> dealerFaceUp) . safeTailThroughNil ) .
        Prelude.filter (isPrefixOf dealerFaceUp ) .
        Prelude.reverse

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


-- Rebuilding for the Set (Card, [Card]) setup.

calcDealerSet :: CardsInPlay -> [Set (Card, Vector Card)] -> Probability
calcDealerSet ( playerCards , [dealerFaceUp] ) =
    \case
        [] ->
            0
        x:xs ->
            go x +
            calcDealerSet (playerCards, [dealerFaceUp]) xs

  where
    go :: Set (Card, Vector Card) -> Probability
    go =
        Prelude.sum .
        Set.toList .
        Set.map
        (
            \v@(firstCard,_) -> probabilityOfDealerHandSet (dealerFaceUp:playerCards) v
        ) 
        .
        Set.filter (\(u,_) -> dealerFaceUp == u )


probabilityOfDealerHandSet :: [Card] -> (Card, Vector Card) -> Probability
probabilityOfDealerHandSet (cardsInPlay) (first,(Vec.toList -> rest)) =
    case rest of

        [] -> 1
        (Ten_Jack_Queen_King:rest) ->
            probabilityTenJackQueenKing cardsInPlay *
            probabilityOfDealerHandSet (Ten_Jack_Queen_King:cardsInPlay) (first,Vec.fromList rest)
        (otherCard:rest) -> 
            probabilityOther cardsInPlay otherCard *
            probabilityOfDealerHandSet (otherCard:cardsInPlay) (first,Vec.fromList rest)