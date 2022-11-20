{-# LANGUAGE OverloadedLists #-}

module CalculateTwoToAce where
    
import CalculateTypes (Card(..))
import Data.Vector ( Vector )
import Data.Set (Set)

-- | The twoToAce terms to specify all ranks considered.
-- Reused by multiple modules.

twoToAce :: Vector Card
twoToAce =
    [Two .. Ace]


twoToAceSet :: Set Card
twoToAceSet =
    [Two .. Ace]