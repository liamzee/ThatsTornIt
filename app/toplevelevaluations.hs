module TopLevelEvaluations where

import CalculateTypes
    ( Action(Surrender, Split, DoubleAction, Hit, Stand),
      BoardState,
      EVAction )
import CalculateEV
    ( calculateHit, calculateSplit, calculateDouble )
import CalculateStand
    ( calculateStand )

--evaluation functions for top-level game conditions.
--Note that evalHitStand will be called by calculateHit.



evaluateSplitDoubleSurrender :: BoardState -> EVAction
evaluateSplitDoubleSurrender boardState =
    maximum
        [
            (calculateSplit boardState, Split),
            (calculateDouble boardState, DoubleAction),
            surrender,
            evalHitStand boardState
        ]


evaluateDoubleSurrender :: BoardState -> EVAction
evaluateDoubleSurrender boardState =
    maximum
        [
            (calculateDouble boardState, DoubleAction),
            surrender,
            evalHitStand boardState
        ]


evaluateDouble :: BoardState -> EVAction
evaluateDouble boardState =
    max (calculateDouble boardState, DoubleAction) (evalHitStand boardState)

    
evalHitStand :: BoardState -> EVAction
evalHitStand boardState =
    max (calculateHit boardState, Hit) (calculateStand boardState, Stand)
    

surrender :: EVAction
surrender = (-0.50, Surrender)
