{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Types where
import GHC.Generics (Generic)
import Control.Parallel.Strategies (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Vector (Vector)

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

    deriving (Show, Generic, Eq, Ord, Enum, NFData)

instance FromJSON Card
instance ToJSON Card


data Action
    = Stand
    | Hit
    | Surrender
    | DoubleAction
    | Split

    deriving (Show, Generic, Eq, Ord, NFData )

instance FromJSON Action
instance ToJSON Action


data AllowedActions
    = ActionsSplitSurrenderDouble
    | ActionsSurrenderDouble
    | ActionsDouble
    | HitStandOnly

    deriving (Show, Generic, Eq, Ord, NFData )

instance FromJSON AllowedActions
instance ToJSON AllowedActions


type BoardPosition = (PlayerCards, DealerFaceUp)

newtype GameState
    = GameState

    {
      gameState :: BoardPosition
    }

    deriving (Show, Generic , Eq , Ord, NFData )

instance FromJSON GameState
instance ToJSON GameState


newtype BlackjackActionDirectoryTopLevel
    = BlackjackActionDirectoryTopLevel

    {
      mainBranches :: Vector ( GameState , BranchContents )
    }

    deriving (Show, Generic, NFData)

instance FromJSON BlackjackActionDirectoryTopLevel
instance ToJSON BlackjackActionDirectoryTopLevel


type DealerFaceUp = Card
type PlayerCards = Vector Card
type DealerHand = Vector Card


newtype BranchContents
    = BranchContents

    {
      branchContents :: Vector ( GameState , AnnotatedSuggestions )
    }

    deriving ( Show , Generic , Eq , Ord, NFData )

instance FromJSON BranchContents
instance ToJSON BranchContents


newtype AnnotatedSuggestions
    = AnnotatedSuggestions

    {
      annotatedSuggestions :: Vector ( AllowedActions , Suggestion , Probability )
    }

    deriving ( Show , Generic , Eq , Ord , NFData )

instance FromJSON AnnotatedSuggestions
instance ToJSON AnnotatedSuggestions


type Probability = () -- not providing this functionality right now.
type EV = Double

newtype Suggestion
    = Suggestion

    {
      suggestion :: ( EV , Action )
    }

    deriving ( Show , Generic , Eq , Ord , NFData)

instance FromJSON Suggestion
instance ToJSON Suggestion
