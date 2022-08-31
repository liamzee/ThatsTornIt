module ProbabilityCalculator where

import Types (BoardPosition, Card (..), EV)
import Data.Vector (snoc, filter, Vector)


probabilityOfEventCalculator :: Vector Card -> Card -> EV
probabilityOfEventCalculator cardsInPlay TenJackQueenKing =
    fromIntegral
    (
        128 - length 
        (Data.Vector.filter (TenJackQueenKing==) cardsInPlay)
    )
    /
    fromIntegral
    (
        416 - length cardsInPlay
    )
probabilityOfEventCalculator cardsInPlay card =
    fromIntegral
    (
        32 - length
        (Data.Vector.filter (card==) cardsInPlay)
    )
    /
    fromIntegral
    (
        416 - length cardsInPlay
    )