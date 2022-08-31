{-# LANGUAGE OverloadedLists #-}

module GetTotalEV where
import Data.Vector (Vector, (!), fromList)
import Main (Card (..), DealerFaceUp, EV, BoardPosition, tensCalc, otherCalc, dupe, Suggestion (suggestion), evaluateSplitDoubleSurrender, evaluateNoSplitDoubleSurrender, gameStateList)
import qualified Data.Set
import Data.Bifunctor (bimap)

--Going to peek ahead and compute EV, just to set up a system of tests.
--Currently, this test is failing substantially and is revealing an EV higher
--than it should be.

checkEVofGame :: Double
checkEVofGame =
    sum $
    uncurry (*) .
    bimap probabilityOfStartingHandsSplitter applyApplicableEvaluation .
    dupe <$>
    listOfStartingHands


applyApplicableEvaluation :: (Vector Card, DealerFaceUp) -> EV
applyApplicableEvaluation boardState@(playerCards, dealerFaceUp)=
    fst . suggestion $ case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b ->  evaluateSplitDoubleSurrender boardState
        _ -> evaluateNoSplitDoubleSurrender boardState


listOfStartingHands :: Vector (Vector Card, Card)
listOfStartingHands =  Data.Vector.fromList . Data.Set.toList $ Data.Set.filter ((==2).length.fst) gameStateList


probabilityOfStartingHandsSplitter :: BoardPosition -> Double
probabilityOfStartingHandsSplitter boardPosition@(playerCards, dealerFaceUp) =
    case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b -> probabilityOfStartingHands boardPosition
        _ -> 2 * probabilityOfStartingHands boardPosition


probabilityOfStartingHands :: BoardPosition -> Double
probabilityOfStartingHands boardPosition@(playerCards, dealerFaceUp) =
    internalSplitter (playerCards Data.Vector.! 0) [] *
    internalSplitter (playerCards Data.Vector.! 1) [playerCards Data.Vector.! 0] *
    internalSplitter dealerFaceUp playerCards

  where

    internalSplitter TenJackQueenKing cardsInPlay =
        tensCalc cardsInPlay
    internalSplitter other cardsInPlay =
        otherCalc cardsInPlay other