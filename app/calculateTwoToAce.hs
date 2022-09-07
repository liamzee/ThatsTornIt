{-# LANGUAGE OverloadedLists #-}

module CalculateTwoToAce where
    
import CalculateTypes (Card(..))
import Data.Vector

-- | The twoToAce terms to specify all ranks considered.
-- Reused by multiple modules.

twoToAce :: Vector Card
twoToAce =
    [Two .. Ace]