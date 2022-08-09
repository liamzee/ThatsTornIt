module CardValueChecker where

import DataDeclarations


-- | cardsToValue is a simplified version of a different set of functions,
-- employing an accumulator pattern in hopes of reducing inefficiency.
-- Likewise, this pattern directly pattern-matches on Card instead of using
-- fromEnum. Hope this works!

cardsToValue :: [Card] -> Int
cardsToValue cards = go cards 0 0

  where

    go :: [Card] -> Int -> Int -> Int
    go [] aces otherValue 

        | aces + otherValue > 21 = aces + otherValue
        | aces * 11 + otherValue > 21 = go [] (aces-1) (otherValue+1)
        | otherwise = aces * 11 + otherValue

    go (x:xs) aces otherValue = case x of

        Ace -> go xs (aces+1) $ otherValue
        Two -> go xs aces $ otherValue + 2
        Three -> go xs aces $ otherValue + 3
        Four -> go xs aces $ otherValue + 4
        Five -> go xs aces $ otherValue + 5
        Six -> go xs aces $ otherValue + 6
        Seven -> go xs aces $ otherValue + 7
        Eight -> go xs aces $ otherValue + 8
        Nine -> go xs aces $ otherValue + 9
        Ten_Jack_Queen_King -> go xs aces $ otherValue + 10


-- | Mostly avoids a direct call to cardsToValue, otherwise mostly identical to directly
-- calling the Boolean. Note the invisible [Card] via eta reduction.

valueCheck :: Int -> (Int -> Int -> Bool) -> [Card] -> Bool
valueCheck value boolType = boolType value . cardsToValue 