

module DealerHands where

import DataDeclarations
import CardValueChecker
import CommonNamesAndFunctions
import Control.Applicative ((<**>))



--Dealer hands aliases for non-Six-Card-Charlie. Also useful for testing.



-- | Generally all dealerHands that don't trigger Six-Card-Charlie.

dealerHandsNotSix :: [DealerCards]
dealerHandsNotSix = filter ( (/= 6 ) . length ) $ dealerHands



-- Specific, individual subsets of dealerHands that don't have Six-Card-Charlie

dealerHands21 :: [DealerCards]
dealerHands21 = filter (valueCheck 21 (==) ) dealerHandsNotSix

dealerHands20 :: [DealerCards]
dealerHands20 = filter (valueCheck 20 (==) ) dealerHandsNotSix

dealerHands19 :: [DealerCards]
dealerHands19 = filter (valueCheck 19 (==) ) dealerHandsNotSix

dealerHands18 :: [DealerCards]
dealerHands18 = filter (valueCheck 18 (==) ) dealerHandsNotSix

dealerHands17 :: [DealerCards]
dealerHands17 = filter (valueCheck 17 (==) ) dealerHandsNotSix

dealerHandsNatural :: [DealerCards]
dealerHandsNatural = natural



--Dealer Six-Card-Charlie hands

-- | Dealer Six-Card-Charlie hands.

dealerHandsSix :: [DealerCards]
dealerHandsSix = filter ( (==6) . length ) dealerHands



{- The subsequent functions generate the term "dealerHands", which contains all possible dealer hands. -}



-- | Dealer hands. Iterate creates a list on which elements of the list are repetitions
-- of the action x times, where x is the index of the list starting from 0.

dealerHands :: [DealerCards]
dealerHands = iterate appendNewCardDealer (pure <$> allRanks) !! 5



-- | Appends a new card to a set of cards, but starts by splitting
-- the existing deck into the parts where a dealer won't hit
-- (under stand on soft 17) and parts where a dealer would hit,
-- "filter valueCheck 17"

appendNewCardDealer :: [[Card]] -> [[Card]]
appendNewCardDealer preExistingCards =

    let dealerStandCards = filter ( valueCheck 17 (<=) ) preExistingCards
        dealerHitCards =
          
            filter ( valueCheck 21 (>=) ) $
            ( filter ( valueCheck 17 (>) ) preExistingCards ) <**>
            (flip (++) <$> pure <$> allRanks) in

          --Last value filters preExistingCards based on player hitting,
          --starting at the end. The ((:) <$> deck) <*> adds cards, then
          --the filter before it removes combinations that would cause the
          --dealer to go bust.

    dealerStandCards <> dealerHitCards