{-# LANGUAGE OverloadedLists #-}

module TopLevelEvaluator where


import DataDeclarations
    ( Suggestion,
      Probability,
      CardsInPlay,
      Action(Split, Surrender, DoubleAction) )
import HitStandEvaluator (evaluateHitVsStand)
import CommonNamesAndFunctions (appendNewCardPlayer, probabilityOfPlayerDraw, safeInitThroughNil)
import StandEVEvaluator ( calculateStandEV )
import Data.Sequence
import qualified Data.Sequence as Seq


-- Evaluation functions that provide support for the top-level, wherein more than the usual
-- split vs hit options exist.



evaluateInitial :: CardsInPlay -> Suggestion
evaluateInitial cardsInPlay = case cardsInPlay of
    ( [firstCard , secondCard], _ ) | firstCard == secondCard ->
        maximum $ optionsWithSplit cardsInPlay
    _ -> maximum $ optionsWithoutSplit cardsInPlay

optionsWithSplit :: CardsInPlay -> Seq Suggestion
optionsWithSplit cardsInPlay =
    [
        doubleCards cardsInPlay ,
        splitCards cardsInPlay ,
        surrender ,
        evaluateHitVsStand cardsInPlay 
    ]

optionsWithoutSplit :: CardsInPlay -> Seq Suggestion
optionsWithoutSplit cardsInPlay =
    [
        doubleCards cardsInPlay ,
        surrender ,
        evaluateHitVsStand cardsInPlay
    ]

optionsWithoutSplitOrSurrender :: CardsInPlay -> [Suggestion]
optionsWithoutSplitOrSurrender cardsInPlay =
    [
        doubleCards cardsInPlay ,
        evaluateHitVsStand cardsInPlay
    ]

surrender :: Suggestion
surrender = ( 0.50 , Surrender )

doubleCards :: CardsInPlay -> Suggestion
doubleCards cardsInPlay = 
    (
        subtract 1 .
        (*2) .
        sum $
        deriveCorrectProbabilityForDouble <$>
        appendNewCardPlayer cardsInPlay
    ,
        DoubleAction
    )

  where

    deriveCorrectProbabilityForDouble :: CardsInPlay -> Probability
    deriveCorrectProbabilityForDouble newCardsInPlay =
        probabilityOfPlayerDraw newCardsInPlay *
        calculateStandEV newCardsInPlay

        

splitCards :: CardsInPlay -> Suggestion
splitCards (playerCards , dealerFaceUp) = 
    
    ( 
        subtract 1 .
        (*2) .
        sum $
        splitProcessor <$>
        appendNewCardPlayer
        (
            safeInitThroughNil playerCards
            ,
            dealerFaceUp
        )
        ,
        Split
    )

  where

    splitProcessor :: CardsInPlay -> Probability
    splitProcessor cardsInPlay =
      
        (
            \(ev,_) ->
                (
                    probabilityOfPlayerDraw cardsInPlay *
                    ev 
                )
        ) $
        maximum $
        optionsWithoutSplitOrSurrender cardsInPlay
