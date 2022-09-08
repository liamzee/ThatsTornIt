module EvaluateSplit where
import CalculateTypes (BoardState, EVAction, Action (DoubleAction), EV)
import Data.Vector (snoc, Vector)
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import qualified Data.Vector as Vec
import Data.List (sort)
import Parallelize (parallelize)
import EvaluateActions (surrender)
import CalculateStand (calculateStandInner)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)

--This is prototype code, under the present conditions it's pretty obvious I can't
--optimize this code enough to be performant within reasonable time.


evaluateSplitResult :: BoardState -> EVAction
evaluateSplitResult boardState = 
    max
        (calculateDoubleAfterFirstSplit boardState, DoubleAction)
        (evaluateHitStandAfterFirstSplit boardState)


calculateDoubleAfterFirstSplit :: BoardState -> EV
calculateDoubleAfterFirstSplit boardState = undefined


evaluateHitStandAfterFirstSplit :: BoardState -> EVAction
evaluateHitStandAfterFirstSplit boardState = undefined


calculateStandAfterFirstSplit :: BoardState -> EV
calculateStandAfterFirstSplit boardState = undefined


boardStatesAfterGame :: BoardState -> Vector BoardState
boardStatesAfterGame boardState@(playerCards,dealerFaceUp,removedCards) =
    let cardsInPlay =
            Vec.fromList . sort . Vec.toList $
            playerCards <> removedCards `snoc` dealerFaceUp
        cardsInPlayToRemovedHands (inputPlayerHands ,inputDealerFaceUp , _) =
            (inputPlayerHands , inputDealerFaceUp, cardsInPlay)
            in
    (cardsInPlayToRemovedHands <$> allNonSplitBoardStates)

evaluateDoubleSplitSurrenderAfterSecondSplit :: BoardState -> EVAction
evaluateDoubleSplitSurrenderAfterSecondSplit boardState =
    maximum
        [
            (calculateDoubleAfterSecondSplit boardState, DoubleAction),
            surrender,
            evaluateHitStandAfterSecondSplit boardState
        ]

standEVMapAfterSecondSplit :: BoardState -> Map BoardState EV
standEVMapAfterSecondSplit boardState =
    parallelize (boardStatesAfterGame boardState) calculateStandInner



calculateDoubleAfterSecondSplit = undefined


evaluateHitStandAfterSecondSplit = undefined