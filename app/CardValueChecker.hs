module CardValueChecker where

import DataDeclarations ( Card(ReducedAce, Ace) )


-- | The next five functions, focusing on cardsToValue, derive the hand value.
-- Note the mutual recursion involved; cardsToValue calls checkForAces, checkForAces
-- calls cardsToValue. This strips aces, one by one, as necessary, to attempt to push
-- the value below 21. If it still fails, checkForAces triggers 22 and breaks out of the
-- recursive loop.
-- Note that ReducedAce never actually appears in a hand, only as an item existing within
-- the card value calculation system.
-- valueCheck, likewise, provides a way to do an int boolean check with cardsToValue.

cardsToValue :: [Card] -> Int
cardsToValue cards =
  
    if basicValueOfCards cards > 21
        then checkForAces cards
        else basicValueOfCards cards



-- | Mostly avoids a direct call to cardsToValue, otherwise mostly identical to directly
-- calling the Boolean. Note the invisible [Card] via eta reduction.

valueCheck :: Int -> (Int -> Int -> Bool) -> [Card] -> Bool
valueCheck value boolType = boolType value . cardsToValue 
  


-- | Note fromEnum requires (+1) due to the absence of a zero value in cards.

basicValueOfCards :: [Card] -> Int
basicValueOfCards = sum . fmap ( (+1) . fromEnum ) 



-- | As mentioned above, checkForAces is in a mutually-recursive loop with cardsToValue,
-- breaking out via a declaration of 22. It calls reduceAce to reduce aces one by one from
-- 11 to 1 in value. Using 22 to indicate bust is admittedly a bit inelegant.



checkForAces :: [Card] -> Int
checkForAces cards =
  
    if elem Ace cards
        then cardsToValue $ reduceAce cards
        else 22


  
-- | As before, note that reduceAce never actually creates a ReducedAce in a hand, only
-- for cardsToValue, checkForAces, etc, for calculation purposes. Note that this only reduces
-- one ace. Other such functions might reduce all aces.

reduceAce :: [Card] -> [Card]
reduceAce hand = case hand of

    [] -> []
    Ace : xs -> ReducedAce:xs
    x : xs -> x : reduceAce xs