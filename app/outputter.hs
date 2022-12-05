module Outputter where
import Data.ByteString.Lazy (ByteString)
import Data.Aeson (encode)
import JSONTypes (Entries(Entry), TopLevels (TopLevel),)
import TotalEVTester (listOfStartingHands)
import CalculateTypes (BoardState, Action(NotApplicable), Card, EV)
import Data.Vector (Vector, (!))
import EvaluateActions (evaluateSplitDoubleSurrender, evaluateDoubleSurrender, evaluateHitStand)
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import qualified Data.Vector as Vec
import qualified Data.Set as Set
import Data.Map (Map)

packedJSON :: Map (Card, Card, Vector Card) EV -> ByteString
packedJSON mapping = encode $ responseList mapping

responseList :: Map (Card, Card, Vector Card) EV -> Vector TopLevels
responseList mapping = makeTopLevels <$> listOfStartingHands
    where
        makeTopLevels :: BoardState -> TopLevels
        makeTopLevels boardState@(playerCards, dealerFaceUp) =
            TopLevel
                playerCards
                dealerFaceUp
                (if playerCards ! 0 == playerCards ! 1 then evaluateSplitDoubleSurrender mapping boardState else (0, NotApplicable))
                (evaluateDoubleSurrender boardState)
            $
            Set.toList . Set.map makeEntries $
            Set.filter
                (\(pCards, dFaceUp) -> playerCards == Vec.take 2 pCards && dealerFaceUp == dFaceUp)
                allNonSplitBoardStates
        
        makeEntries :: BoardState -> Entries
        makeEntries boardState@(playerCards, dealerFaceUp) =
            Entry dealerFaceUp playerCards $ evaluateHitStand boardState
