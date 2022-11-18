module Outputter where
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (encode)
import JSONTypes (Entries(Entry), TopLevels (TopLevel),)
import TotalEVTester (listOfStartingHands)
import CalculateTypes (BoardState, Action(NotApplicable))
import Data.Vector (Vector, (!))
import EvaluateActions (evaluateSplitDoubleSurrender, evaluateDoubleSurrender, evaluateHitStand)
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import qualified Data.Vector as Vec
import BoardStatesSeed (seedBoardStates)

packedJSON :: ByteString
packedJSON = encode responseList

responseList :: Vector TopLevels
responseList = makeTopLevels <$> listOfStartingHands
    where
        makeTopLevels :: BoardState -> TopLevels
        makeTopLevels boardState@(playerCards, dealerFaceUp, removedCards) =
            TopLevel
                playerCards
                dealerFaceUp
                (if playerCards ! 0 == playerCards ! 1 then evaluateSplitDoubleSurrender boardState else (0, NotApplicable))
                (evaluateDoubleSurrender boardState)
            $
            makeEntries <$>
            Vec.filter
                (\(pCards, dFaceUp, removedCards) -> playerCards == Vec.take 2 pCards && dealerFaceUp == dFaceUp)
                allNonSplitBoardStates
        
        makeEntries :: BoardState -> Entries
        makeEntries boardState@(playerCards, dealerFaceUp, removedCards) =
            Entry dealerFaceUp playerCards $ evaluateHitStand boardState