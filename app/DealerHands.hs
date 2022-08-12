{-# LANGUAGE OverloadedLists #-}

module DealerHands where

import DataDeclarations
import CardValueChecker
import CommonNamesAndFunctions
import Control.Applicative ((<**>))
import Data.Sequence
import qualified Data.Sequence as Seq



--Dealer hands aliases for non-Six-Card-Charlie. Also useful for testing.



-- | Generally all dealerHands that don't trigger Six-Card-Charlie.

dealerHandsNotSix :: Seq DealerCards
dealerHandsNotSix = Seq.filter ( (/= 6 ) . Prelude.length ) $ dealerHands

dealerHandsNotSixGen :: Card -> Seq DealerCards
dealerHandsNotSixGen card = Seq.filter ( (/=6) . Prelude.length ) $ dealerHandsGen card


-- Specific, individual subsets of dealerHands that don't have Six-Card-Charlie

dealerHandsNatural :: Seq DealerCards
dealerHandsNatural = natural

dealerHands21 :: Seq DealerCards
dealerHands21 = Seq.filter ( valueCheck 21 (==)) . Seq.filter ( (==6) . Prelude.length ) $ dealerHands

dealerHands21Gen :: Card -> Seq DealerCards
dealerHands21Gen card = Seq.filter ( valueCheck 21 (==)) . Seq.filter ( (==6) . Prelude.length ) $ dealerHandsGen card


--Dealer Six-Card-Charlie hands

-- | Dealer Six-Card-Charlie hands.

dealerHandsSix :: Seq DealerCards
dealerHandsSix = Seq.filter ( (==6) . Prelude.length ) dealerHands

dealerHandsSixGen :: Card -> Seq DealerCards
dealerHandsSixGen card = Seq.filter ( (==6) . Prelude.length ) ( dealerHandsGen card )



{- The subsequent functions generate the term "dealerHands", which contains all possible dealer hands. -}



-- | Dealer hands. Iterate creates a list on which elements of the list are repetitions
-- of the action x times, where x is the index of the list starting from 0.

dealerHands :: Seq DealerCards
dealerHands = iterate appendNewCardDealer (allRanksNested) !! 5

dealerHandsGen :: Card -> Seq DealerCards
dealerHandsGen specificCard = iterate appendNewCardDealer ([[specificCard]]) !! 5



-- | Appends a new card to a set of cards, but starts by splitting
-- the existing deck into the parts where a dealer won't hit
-- (under stand on soft 17) and parts where a dealer would hit,
-- "filter valueCheck 17"

appendNewCardDealer :: Seq [Card] -> Seq [Card]
appendNewCardDealer preExistingCards =

    let dealerStandCards = Seq.filter ( valueCheck 17 (<=) ) preExistingCards

        dealerHitCards =
          
            Seq.filter ( valueCheck 21 (>=) ) $
            Seq.filter ( valueCheck 17 (>) ) preExistingCards <**>
            (
                flip (<>) <$> allRanksNested
            ) 
            
            in

          --Last value filters preExistingCards based on player hitting,
          --starting at the end. The ((:) <$> deck) <*> adds cards, then
          --the filter before it removes combinations that would cause the
          --dealer to go bust.

    dealerStandCards <> dealerHitCards