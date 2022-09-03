{-# LANGUAGE DeriveAnyClass, DeriveGeneric, PatternSynonyms #-}
module CalculateTypes where

import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)
import Data.Vector


data Card
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | TenJackQueenKing
    | Ace
    deriving (Eq, Ord, Enum, Generic, Show, NFData)

pattern Tens :: Card
pattern Tens = TenJackQueenKing --for debugging, convenience.
    

data Action
    = Hit
    | Stand
    | Split
    | DoubleAction
    | Surrender
    deriving (Eq, Ord, Generic, Show, NFData)


type BoardState = (PlayerCards, DealerFaceUp, RemovedCards)
type PlayerCards = Vector Card
type DealerFaceUp = Card
type RemovedCards = Vector Card

type EV = Double
type EVAction = (EV, Action)