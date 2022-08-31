{-# LANGUAGE OverloadedLists #-}
module TopLevelCalculations where

import Types ( Suggestion(Suggestion), Action(..), EV )

--evaluation functions for game conditions:

{-

evaluateSplitDoubleSurrender :: gameState -> Suggestion
evaluateSplitDoubleSurrender boardPosition =
    maximum [(calculateSplit boardPosition, Split), (calculateDouble boardPosition, DoubleAction), surrender, evalHitStand boardPosition]


evaluateDoubleSurrender :: gameState -> Suggestion
evaluateDoubleSurrender boardPosition =
    maximum [(calculateDouble boardPosition, DoubleAction), surrender, evalHitStand boardPosition]


evaluateDouble :: gameState -> (EV, Action)
evaluateDouble boardPosition =
    max (calculateDouble gameState, DoubleAction) (evalHitStand gameState)

evalHitStand :: gameState -> Suggestion
evalHitStand = undefined

surrender :: Suggestion
surrender = Suggestion (-0.50, Surrender)

-}