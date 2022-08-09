{-# LANGUAGE PatternSynonyms, DeriveGeneric #-}

module DataDeclarations where

import Data.Aeson
import GHC.Generics

--Basic types, type synonyms.

data Action =

      Split
    | DoubleAction
    | Surrender
    | Hit
    | Stand

    deriving (Generic, Eq, Ord, Show)

data Card =

      ReducedAce
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten_Jack_Queen_King
    | Ace
    
    deriving (Generic, Eq, Enum, Show, Ord)

instance FromJSON Action
instance FromJSON Card
instance ToJSON Action
instance ToJSON Card

-- Pattern synonym for Ten_Jack_Queen_King added for debugging purposes.

pattern Tens :: Card
pattern Tens = Ten_Jack_Queen_King

type CardsInPlay = ( PlayerCards , DealerCards )

type PlayerCards = [ Card ]

type DealerCards = [ Card ]

type CardsRevealed = [ Card ]

type Players = [ Card ]

type Probability = Double

type ExpectedValue = Double

type Suggestion = (ExpectedValue, Action)


-- JSON types


newtype BlackjackSuggestions =

    BlackjackSuggestions
    {
    topLevels :: [(Seed, GameTreeContents)]
    }

    deriving (Generic, Show)

newtype GameTreeContents =

    GameTreeContents
    {
    gameTreeContents :: [Suggestion]
    }

    deriving (Generic, Show)

newtype Seed =

    Seed
    {
    seed :: CardsInPlay
    }

    deriving (Generic, Show, Eq, Ord)

instance FromJSON BlackjackSuggestions
instance ToJSON BlackjackSuggestions
instance FromJSON GameTreeContents
instance ToJSON GameTreeContents
instance FromJSON Seed
instance ToJSON Seed
