{-# LANGUAGE DeriveGeneric #-}
module JSONTypes where

import CalculateTypes (PlayerCards, DealerFaceUp, EVAction, EV, Action, Card)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data TopLevels = TopLevel
    {
        a_tPCards :: PlayerCards
    ,   b_tDFU :: DealerFaceUp
    ,   c_tActionWithSplit :: EVAction
    ,   d_tActionWithoutSplit :: EVAction
    ,   e_normalListOfEntries :: [Entries]
    }
        deriving Generic

instance FromJSON TopLevels
instance ToJSON TopLevels

data Entries = Entry
    {
        a_eDFU :: DealerFaceUp
    ,   b_ePCards :: PlayerCards
    ,   c_eHitOrStand :: (EV, Action)
    }
        deriving (Generic, Ord, Eq)

instance FromJSON Entries
instance ToJSON Entries

data Splits = Splits
    {
        a_sDFU :: DealerFaceUp
    ,   b_sPlayerSplitCard :: Card
    ,   c_sPlayerCards :: PlayerCards
    ,   d_doubleHitStand :: EVAction
    ,   e_firstSplitListOfEntries :: [Entries]
    }   
        deriving Generic

instance FromJSON Splits
instance ToJSON Splits