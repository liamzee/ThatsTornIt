module CardValueChecker where

import DataDeclarations
import Data.Set
import Data.Vector
import qualified Data.Vector as Vec



-- | cardsToValue is a simplified version of a different set of functions,
-- employing an accumulator pattern in hopes of reducing inefficiency.
-- Likewise, this pattern directly pattern-matches on Card instead of using
-- fromEnum. Hope this works!

cardsToValue :: [Card] -> Int
cardsToValue cards = cardsToValueInner cards 0 0


cardsToValueInner :: [Card] -> Int -> Int -> Int
cardsToValueInner [] aces otherValue 
        | aces + otherValue > 21 =
              aces + otherValue
        | aces * 11 + otherValue > 21 =
              cardsToValueInner [] (aces-1) (otherValue+1)
        | otherwise =
              aces * 11 + otherValue
cardsToValueInner input aces otherValue =
      case input of
        Ace:xs ->
            cardsToValueInner xs (aces+1) otherValue
        Two:xs ->
            cardsToValueInner xs aces (otherValue + 2)
        Three:xs ->
            cardsToValueInner xs aces (otherValue + 3)
        Four:xs ->
            cardsToValueInner xs aces (otherValue + 4)
        Five:xs ->
            cardsToValueInner xs aces (otherValue + 5)
        Six:xs ->
            cardsToValueInner xs aces (otherValue + 6)
        Seven:xs ->
            cardsToValueInner xs aces (otherValue + 7)
        Eight:xs ->
            cardsToValueInner xs aces (otherValue + 8)
        Nine:xs ->
            cardsToValueInner xs aces (otherValue + 9)
        Ten_Jack_Queen_King:xs ->
            cardsToValueInner xs aces (otherValue + 10)


cardsToValueSet :: (Card, [Card]) -> Int
cardsToValueSet (card, cards) =
    case card of
        Ace ->
            cardsToValueInner (cards) 1 0
        Two ->
            cardsToValueInner (cards) 0 2
        Three ->
            cardsToValueInner (cards) 0 3
        Four ->
            cardsToValueInner (cards) 0 4
        Five ->
            cardsToValueInner (cards) 0 5
        Six ->
            cardsToValueInner (cards) 0 6
        Seven ->
            cardsToValueInner (cards) 0 7
        Eight ->
            cardsToValueInner (cards) 0 8
        Nine ->
            cardsToValueInner (cards) 0 9
        Ten_Jack_Queen_King ->
            cardsToValueInner (cards) 0 10


cardsToValueSetVector :: (Card, Vector Card) -> Int
cardsToValueSetVector (card, cards) =
    case card of
        Ace ->
            cardsToValueInner (Vec.toList cards) 1 0
        Two ->
            cardsToValueInner (Vec.toList cards) 0 2
        Three ->
            cardsToValueInner (Vec.toList cards) 0 3
        Four ->
            cardsToValueInner (Vec.toList cards) 0 4
        Five ->
            cardsToValueInner (Vec.toList cards) 0 5
        Six ->
            cardsToValueInner (Vec.toList cards) 0 6
        Seven ->
            cardsToValueInner (Vec.toList cards) 0 7
        Eight ->
            cardsToValueInner (Vec.toList cards) 0 8
        Nine ->
            cardsToValueInner (Vec.toList cards) 0 9
        Ten_Jack_Queen_King ->
            cardsToValueInner (Vec.toList cards) 0 10
            


-- | Mostly avoids a direct call to cardsToValue, otherwise mostly identical to directly
-- calling the Boolean. Note the invisible [Card] via eta reduction.

--valueCheck :: ByteString -> (ByteString -> ByteString -> Bool) -> [Card] -> ByteString
valueCheck :: Int -> (Int -> Int -> Bool) -> [Card] -> Bool
valueCheck value boolType = boolType value . cardsToValue 

valueCheckSet :: Int -> (Int -> Int -> Bool) -> (Card, [Card]) -> Bool
valueCheckSet value boolType = boolType value . cardsToValueSet

valueCheckSetVector :: Int -> (Int -> Int -> Bool) -> (Card, Vector Card) -> Bool
valueCheckSetVector value boolType = boolType value . cardsToValueSetVector