{-# LANGUAGE DeriveAnyClass #-}
module CalculateTypes where

import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)
import Data.Sequence ( Seq )


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
    

data Action
    = Hit
    | Stand
    | Split
    | DoubleAction
    | Surrender
    deriving (Eq, Ord, Generic, Show, NFData)


type BoardState = (PlayerCards, DealerFaceUp, RemovedCards)
type PlayerCards = Seq Card
type DealerFaceUp = Card
type RemovedCards = Seq Card

type EV = Double
type EVAction = (EV, Action)