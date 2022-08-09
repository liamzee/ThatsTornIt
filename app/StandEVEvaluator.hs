
module StandEVEvaluator where

import DataDeclarations
    ( ExpectedValue,
      Probability,
      DealerCards,
      CardsInPlay,
      Card(Ten_Jack_Queen_King) )
import CommonNamesAndFunctions
    ( natural,
      safeTailThroughNil,
      probabilityTenJackQueenKing,
      probabilityOther )
import DealerHands
    ( dealerHandsNotSix,
      dealerHands21,
      dealerHandsNatural,
      dealerHandsSix )
import CardValueChecker ( cardsToValue, valueCheck )
import Data.List ((\\), isPrefixOf)


{- The following functions calculate the stand EV, which is fundamental for
determining the EV of an action.

Ultimately, all actions eventually recourse to what the stand EV is, because if
  you no longer have valid moves, you must stand.-}



calculateStandEV :: CardsInPlay -> ExpectedValue
calculateStandEV cardsInPlay@( playerCards , dealerFaceUp ) 
  
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
  
    | length playerCards == 6 = playerSixCardCharlieCalc
  
    | elem playerCards natural =
        
        2.5 - 1.5 * calcDealer cardsInPlay dealerHandsNatural
  
    | playerHandValue == 21 = playerNonNatural21Calc
  
    | otherwise = playerNormalCalc
  
  where
  
  
  
    playerHandValue :: Int
    playerHandValue = cardsToValue playerCards
  
  
      
    playerSixCardCharlieCalc :: ExpectedValue
    playerSixCardCharlieCalc = 
        
        2
        - 2 * lossConditionEV
        -  tieConditionEV

        where lossConditionEV =
          
                  calcDealer cardsInPlay dealerHandsNatural
                  + calcDealer cardsInPlay 
                  (filter (valueCheck playerHandValue (<)) dealerHandsSix)

              tieConditionEV =

                  calcDealer cardsInPlay $
                  filter (valueCheck playerHandValue (==)) dealerHandsSix
  
  
  
    playerNonNatural21Calc :: ExpectedValue
    playerNonNatural21Calc =
  
        2
        - 2 * lossConditionEV
        - tieConditionEV

      where

        lossConditionEV = 
          
          calcDealer cardsInPlay dealerHandsSix
          + calcDealer cardsInPlay dealerHandsNatural

        tieConditionEV =

          calcDealer cardsInPlay dealerHands21
          - calcDealer cardsInPlay dealerHandsNatural
  
  
  
    playerNormalCalc :: ExpectedValue
    playerNormalCalc =
  
        2 - 2 * lossConditionEV
        - tieConditionEV
        

      where

        lossConditionEV = 

            calcDealer cardsInPlay dealerHandsSix +
            calcDealer cardsInPlay
            (filter (valueCheck playerHandValue (<)) dealerHandsNotSix)

        tieConditionEV =
          
          calcDealer cardsInPlay $
          filter (valueCheck playerHandValue (==)) dealerHandsNotSix
  
  
  
calcDealer :: CardsInPlay -> [DealerCards] -> Probability
calcDealer ( playerCards , dealerFaceUp ) =
    
    sum . map (probabilityOfDealerHand playerCards . safeTailThroughNil ) .
    filter (isPrefixOf $ dealerFaceUp )
  
  
  
probabilityOfDealerHand :: [Card] -> DealerCards -> Probability
probabilityOfDealerHand cardsInPlay dealerCards = case dealerCards of
      
    [] -> 1
    Ten_Jack_Queen_King:xs -> probabilityTenJackQueenKing cardsInPlay *
        probabilityOfDealerHand (Ten_Jack_Queen_King:cardsInPlay) xs
    otherCard:xs -> probabilityOther cardsInPlay otherCard *
        probabilityOfDealerHand (otherCard:cardsInPlay) xs