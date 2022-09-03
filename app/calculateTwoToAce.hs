{-# LANGUAGE OverloadedLists #-}

module CalculateTwoToAce where
    
import CalculateTypes (Card(..))
import Data.Vector

twoToAce :: Vector Card
twoToAce =
    [Two .. Ace]