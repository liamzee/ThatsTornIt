module HitStandEvaluator where

import DataDeclarations
import CommonNamesAndFunctions
    ( appendNewCardPlayer, probabilityOfPlayerDraw )
import StandEVEvaluator (calculateStandEV)
import Data.Sequence


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
evaluateHits :: Suggestion -> Seq Suggestion -> Suggestion
evaluateHits stand Empty = stand
evaluateHits stand hit = max stand (sum $ fst <$> hit, Hit)



calculateStands :: CardsInPlay -> Suggestion
calculateStands cardsInPlay = ( calculateStandEV cardsInPlay , Stand )
