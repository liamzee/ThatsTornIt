{-# LANGUAGE OverloadedLists, ApplicativeDo #-}

module DealerHands where

import DataDeclarations ( Card (..), DealerCards )
import CardValueChecker ( valueCheck )
import CommonNamesAndFunctions ( allRanks )
import Control.Applicative ((<**>))


--Dealer hands aliases for non-Six-Card-Charlie. Also useful for testing.



-- | Generally all dealerHands that don't trigger Six-Card-Charlie.

dealerHandsNotSix :: [DealerCards]
dealerHandsNotSix = iterate appendDealerCard (pure <$> allRanks) !! 4

-- Specific, individual subsets of dealerHands that don't have Six-Card-Charlie

dealerHands21 :: [DealerCards]
dealerHands21 = filter (valueCheck 21 (==)) $ dealerHandsNotSix

--Dealer Six-Card-Charlie hands

-- | Dealer Six-Card-Charlie hands.

dealerHandsSix :: [DealerCards]
dealerHandsSix = appendDealerCardSix dealerHandsNotSix

-- | Stuff that creates lists of dealer hands from existing dealer hands.

appendDealerCard :: [DealerCards] -> [DealerCards]
appendDealerCard preexistingCards =
    do
        oldHand <- preexistingCards
        if valueCheck 17 (<=) oldHand
            then [oldHand]
            else do
                newCard <- allRanks
                if valueCheck 21 (<) (newCard:oldHand)
                    then []
                    else [oldHand <> [newCard]]


appendDealerCardSix :: [DealerCards] -> [DealerCards]
appendDealerCardSix preexistingCards =
    do
        oldHand <- preexistingCards
        newCard <- allRanks
        if 5 /= length oldHand
            then []
            else if valueCheck 21 (<) (newCard:oldHand)
                then []
                else [oldHand <> [newCard]]