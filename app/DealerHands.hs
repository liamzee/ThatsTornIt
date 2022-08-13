{-# LANGUAGE OverloadedLists, ApplicativeDo #-}

module DealerHands where

import DataDeclarations ( Card (..), DealerCards )
import CardValueChecker ( valueCheck, valueCheckSet, valueCheckSetVector )
import CommonNamesAndFunctions ( allRanks )
import Control.Applicative ((<**>))
import Data.Set
import qualified Data.Set as Set
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Vector
import qualified Data.Vector as Vec


--Dealer hands aliases for non-Six-Card-Charlie. Also useful for testing.



-- | Generally all dealerHands that don't trigger Six-Card-Charlie.

dealerHandsNotSix :: [DealerCards]
dealerHandsNotSix = iterate appendDealerCard (pure <$> allRanks) !! 4

-- Specific, individual subsets of dealerHands that don't have Six-Card-Charlie

dealerHands21 :: [DealerCards]
dealerHands21 = Prelude.filter (valueCheck 21 (==)) $ dealerHandsNotSix

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
        if 5 /= Prelude.length oldHand || valueCheck 17 (<=) oldHand
            then []
            else do
                newCard <- allRanks
                if valueCheck 21 (<) (newCard:oldHand)
                    then []
                    else [oldHand <> [newCard]]



-- Rebuilding everything to use the Set (Card, Set Card) datatype.

core :: Set (Card, Vector Card)
core =
    unions $
    allRanks <&>
    (\elem -> Set.fromList [(elem, [])])


appendDealerCardSet :: Set (Card, Vector Card) -> Set (Card, Vector Card)
appendDealerCardSet preexistingSet =
    Set.fromList $
        do
            (card, set) <- Set.toList preexistingSet
            if valueCheck 17 (<=) (Vec.toList $ cons card set)
                then [(card, set)]
                else do
                    newCard <- allRanks
                    if valueCheck 21 (<) (Vec.toList $ cons card $ cons newCard set)
                        then []
                        else [(card, set <> [newCard] )]


appendDealerCardSetSix :: Set (Card, Vector Card) -> Set (Card, Vector Card)
appendDealerCardSetSix preexistingSet =
    Set.fromList $
        do
            (card, set) <- Set.toList preexistingSet
            if 4 /= Vec.length set || valueCheck 17 (<=) (Vec.toList $ cons card set)
                then []
                else do
                    newCard <- allRanks
                    if valueCheck 21 (<) (Vec.toList $ cons card $ cons newCard set)
                        then []
                        else [(card, set <> [newCard] )]


dealerHandsSetNotSix :: Set (Card, Vector Card)
dealerHandsSetNotSix = iterate appendDealerCardSet core !! 4


dealerHandsSetSix :: Set (Card, Vector Card)
dealerHandsSetSix = appendDealerCardSetSix dealerHandsSetNotSix


dealerHandsSetNotSix21 :: Set (Card, Vector Card)
dealerHandsSetNotSix21 = Set.filter (valueCheckSetVector 21 (<=)) dealerHandsSetNotSix