module HitStandEvaluator where

import DataDeclarations
    ( Action(Stand, Hit), CardsInPlay, Suggestion )
import CommonNamesAndFunctions
    ( appendNewCardPlayer, probabilityOfPlayerDraw )
import StandEVEvaluator (calculateStandEV)
import Data.Sequence ( Seq(Empty) )


{- Terrible performance here, with 8 minute evaluate times on O2.
Needs testing, but seems correct when compared to 
http://www.bjstrat.net/cgi-bin/cdca.cgi . -}

evaluateHitVsStand :: CardsInPlay -> Suggestion
evaluateHitVsStand cardsInPlay =
    evaluateHits
    (   
        calculateStandEV cardsInPlay ,
        Stand
    )
    (
        correctProbabilityCardsInPlay <$>
        appendNewCardPlayer cardsInPlay
    )
  where
    correctProbabilityCardsInPlay :: CardsInPlay -> Suggestion
    correctProbabilityCardsInPlay cardsInPlayNew =
        (
            probabilityOfPlayerDraw cardsInPlayNew *
            (
                fst $
                evaluateHitVsStand cardsInPlayNew
            )
        ,
        Hit
        )


evaluateHits :: Suggestion -> [Suggestion] -> Suggestion
evaluateHits stand hit =
    case hit of
        [] ->
            stand
        hit ->
            max stand (sum $ fst <$> hit, Hit)


