module MakeAllValidBoardStatesSplit where
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import CalculateTypes (BoardState, Card, EV)
import Data.Vector (Vector, filter, head, snoc, toList, fromList, tail, length, group)
import CalculateDealerHands (countedDealerHands)
import qualified Data.Set
import Data.List (sort)
import Parallelize (parallelize)
import CalculateStand (calculateStandInner)
import qualified Data.Map.Lazy as Data.Map.Internal
import Debug.Trace (traceShowId)


realSnuff :: Data.Map.Internal.Map BoardState CalculateTypes.EV
realSnuff = parallelize (kraken =<< allNonSplitBoardStates) (calculateStandInner . traceShowId)
snuff = Data.Vector.length top

top :: Vector (Vector Card)
top = appendDealer =<< allNonSplitBoardStates 

appendDealer :: BoardState -> Vector (Vector Card)
appendDealer (playerCards, dealerFaceUp, removedCards) = do
    newCards <- Data.Vector.tail $ fst <$> Data.Vector.filter ((dealerFaceUp ==) . Data.Vector.head .fst) countedDealerHands
    pure $ fromList $ sort $ toList $ (playerCards <> removedCards `snoc` dealerFaceUp) <> newCards


kraken :: BoardState -> Vector BoardState
kraken boardState@(playerCards, dealerFaceUp, _) = do
    elem <- top
    pure $ traceShowId (playerCards, dealerFaceUp, elem)