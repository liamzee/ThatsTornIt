module BoardStatesSeed where

import CalculateTypes (BoardState)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import CalculateTwoToAce (twoToAce)

seedBoardStates :: Vector BoardState
seedBoardStates = do
    dealerFaceUp <- twoToAce
    firstPlayerCard <- twoToAce
    secondPlayerCard <- twoToAce
    pure (pure firstPlayerCard, dealerFaceUp)