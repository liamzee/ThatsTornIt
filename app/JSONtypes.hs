{-# LANGUAGE DeriveGeneric #-}
module JSONTypes where

import CalculateTypes (PlayerCards, DealerFaceUp, EVAction, EV, Action)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data TopLevels = TopLevel PlayerCards DealerFaceUp EVAction EVAction (Vector Entries) deriving Generic

instance FromJSON TopLevels
instance ToJSON TopLevels

data Entries = Entry DealerFaceUp PlayerCards (EV, Action) deriving Generic

instance FromJSON Entries
instance ToJSON Entries