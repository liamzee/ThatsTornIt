module CalculateDealerHands where

import CalculateTwoToAce (twoToAce)
import CalculateTypes
import Data.Vector ( Vector, snoc )
import qualified Data.Vector as Vec
import Control.Applicative (Applicative(liftA2))
import CalculateHandValue (checkIfBust, checkForSoft17, handValueOf)


dealerHands :: Vector (Vector Card)
dealerHands =
    appendToCore =<< dealerHandsCore

{- used to test.

filterCheck hand =
    not (((not.checkForSoft17) hand && 6 /= Sequ.length hand) || checkIfBust hand || 6 < Sequ.length hand)

-}

dealerHandsCore :: Vector (Vector Card)
dealerHandsCore =
    liftA2 ( snoc . pure ) twoToAce twoToAce


appendToCore :: Vector Card -> Vector (Vector Card)
appendToCore hand
    | 6 == Vec.length hand || checkForSoft17 hand =
        pure hand
    | otherwise =
        checkIfBustNewHand hand =<< twoToAce 
  where
    checkIfBustNewHand :: Vector Card -> Card -> Vector (Vector Card)
    checkIfBustNewHand hand newCard =
        let newHand = hand `snoc` newCard in
        if checkIfBust newHand
            then pure newHand
            else appendToCore newHand

{-
appendNew :: Seq (Seq Card) -> Seq (Seq Card)
appendNew inputHands = do
    oldHand <- inputHands
    newCard <- twoToAce
    pure $ oldHand :|> newCard

dealerHands2 :: Seq (Seq Card)
dealerHands2 = appendNew $ pure <$> twoToAce

dealerHands3 :: Seq (Seq Card)
dealerHands3 = appendNew dealerHands2

dealerHands4 :: Seq (Seq Card)
dealerHands4 = appendNew dealerHands3

dealerHands5 :: Seq (Seq Card)
dealerHands5 = appendNew dealerHands4

dealerHands6 :: Seq (Seq Card)
dealerHands6 = appendNew dealerHands5-}