{-# LANGUAGE LambdaCase #-}
module TTI where

import DataDeclarations
    ( Suggestion,
      ExpectedValue,
      Probability,
      DealerCards,
      CardsInPlay,
      Card(ReducedAce, Two, Ten_Jack_Queen_King, Ace),
      Action(Stand, Surrender, DoubleAction, Hit, Split) )

import CommonNamesAndFunctions
    ( allRanks,
      natural,
      safeTailThroughNil,
      probabilityTenJackQueenKing,
      probabilityOther,
      appendNewCardPlayer,
      probabilityOfPlayerDraw, safeInitThroughNil )
      
import CardValueChecker ( cardsToValue, valueCheck )

import DealerHands
    ( dealerHandsNotSix,
      dealerHands21,
      dealerHandsNatural,
      dealerHandsSix )

import Data.List (intersect, isPrefixOf, (\\))
import Control.Applicative ((<**>))





-- Evaluation functions that provide support for the top-level, wherein more than the usual
-- split vs hit options exist.




evaluateInitial :: CardsInPlay -> Suggestion
evaluateInitial cardsInPlay = case cardsInPlay of
      
        ( [firstCard , secondCard], _ ) | firstCard == secondCard ->
            
            maximum $ optionsWithSplit cardsInPlay

        _ -> maximum $ optionsWithoutSplit cardsInPlay



optionsWithSplit :: CardsInPlay -> [Suggestion]
optionsWithSplit cardsInPlay =
  
  [ doubleCards cardsInPlay , undefined ,
  surrender , evaluateHitVsStand cardsInPlay ]



optionsWithoutSplit :: CardsInPlay -> [Suggestion]
optionsWithoutSplit cardsInPlay =
  
    [ doubleCards cardsInPlay , surrender , evaluateHitVsStand cardsInPlay]



surrender :: Suggestion
surrender = ( 0.50 , Surrender )



doubleCards :: CardsInPlay -> Suggestion
doubleCards cardsInPlay = 
      
    ( (subtract 1) . (*2) . sum $
    deriveCorrectProbabilityForDouble <$>
    appendNewCardPlayer cardsInPlay
    , DoubleAction )

  where

    deriveCorrectProbabilityForDouble newCardsInPlay =

      probabilityOfPlayerDraw newCardsInPlay *
      calculateStandEV newCardsInPlay



splitCards :: CardsInPlay -> [Suggestion]
splitCards (playerCards , dealerFaceUp) = 
    
    maximum $ splitAppender <$> appendNewCardPlayer (safeInitThroughNil 
    playerCards , dealerFaceUp)

  where

    splitAppender cardsInPlay =
      
        (\(ev,action) -> ((probabilityOfPlayerDraw cardsInPlay) * ev , Split)) <$>
         optionsWithoutSplit cardsInPlay






{- Terrible performance here, with 8 minute evaluate times on O2.
Needs testing, but seems correct when compared to 
http://www.bjstrat.net/cgi-bin/cdca.cgi . -}

evaluateHitVsStand :: CardsInPlay -> Suggestion
evaluateHitVsStand cardsInPlay =

    calculateStands cardsInPlay `evaluateHits`
    correctProbabilityCardsInPlay <$>
    appendNewCardPlayer cardsInPlay

  where

    
    
    correctProbabilityCardsInPlay :: CardsInPlay -> Suggestion
    correctProbabilityCardsInPlay cardsInPlayNew =

        ( probabilityOfPlayerDraw cardsInPlayNew *
        ( fst $ evaluateHitVsStand cardsInPlayNew )
        , Hit )



infixl 0 `evaluateHits`
evaluateHits :: Suggestion -> [Suggestion] -> Suggestion
evaluateHits stand [] = stand
evaluateHits stand hit = max stand (sum $ fst <$> hit, Hit)



calculateStands :: CardsInPlay -> Suggestion
calculateStands cardsInPlay = (,) (calculateStandEV cardsInPlay) Stand



{- Finally have the calculateStandEV calculations partially set up. This overall
section is mostly complete, except for the calculateStandEV calculations.-}



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
      
        2 - 2 * calcDealer cardsInPlay
        
        (

        dealerHandsNatural <>
        (filter (valueCheck playerHandValue (<)) dealerHandsSix)

        )

        - calcDealer cardsInPlay
        (filter (valueCheck playerHandValue (==)) dealerHandsSix)



    playerNonNatural21Calc :: ExpectedValue
    playerNonNatural21Calc =

        2 - 2 * calcDealer cardsInPlay
        
        (

        dealerHandsSix <>
        dealerHandsNatural

        )

        - calcDealer cardsInPlay
        ( dealerHands21 \\ dealerHandsNatural )



    playerNormalCalc :: ExpectedValue
    playerNormalCalc =

          2 - 2 * calcDealer cardsInPlay
      
          (

          dealerHandsSix <>
          filter (valueCheck playerHandValue (<)) dealerHandsNotSix

          )

          - calcDealer cardsInPlay
          (filter (valueCheck playerHandValue (==)) dealerHandsNotSix)



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