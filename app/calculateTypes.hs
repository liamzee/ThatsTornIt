{-# LANGUAGE DeriveAnyClass, DeriveGeneric, PatternSynonyms #-}
{-# LANGUAGE LambdaCase, InstanceSigs #-}
module CalculateTypes where

import Control.Parallel.Strategies (NFData)
import GHC.Generics (Generic)
import Data.Vector
import Data.Aeson ( FromJSON, ToJSON )
import Flat
import Data.Ratio (Ratio)

-- | Card type.

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
    deriving (Eq, Ord, Generic, Show, NFData, Flat)

instance Enum Card where
    fromEnum :: Card -> Int
    fromEnum = \case
        Two -> 2
        Three -> 3
        Four -> 4
        Five -> 5
        Six -> 6
        Seven -> 7
        Eight -> 8
        Nine -> 9
        TenJackQueenKing -> 10
        Ace -> 11
    toEnum :: Int -> Card
    toEnum = \case
        1 -> Ace
        2 -> Two
        3 -> Three
        4 -> Four
        5 -> Five
        6 -> Six
        7 -> Seven
        8 -> Eight
        9 -> Nine
        10 -> TenJackQueenKing
        11 -> Ace

instance FromJSON Card
instance ToJSON Card

pattern Tens :: Card
pattern Tens = TenJackQueenKing --for debugging, convenience.
    

data Action
    = Hit
    | Stand
    | Split
    | DoubleAction
    | Surrender
    | NotApplicable
    deriving (Eq, Ord, Generic, Show, NFData)

instance FromJSON Action
instance ToJSON Action


type BoardState = (PlayerCards, DealerFaceUp)
type PlayerCards = Vector Card
type DealerFaceUp = Card
type RemovedCards = Vector Card

type SplitBoardState = (PlayerCards, DealerFaceUp, RemovedCards)

type EV = Double
type Probability = Double
type EVAction = (EV, Action)