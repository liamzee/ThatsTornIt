{-# LANGUAGE OverloadedLists #-}

module CalculateTwoToAce where
    
import CalculateTypes (Card(..))
import Data.Sequence

twoToAce :: Seq Card
twoToAce =
    [Two .. Ace]