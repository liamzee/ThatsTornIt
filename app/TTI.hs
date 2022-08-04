{-# LANGUAGE ViewPatterns #-}
module TTI where

import Control.Category hiding ((.))
import Data.Function ((&))

--Basic types, type synonyms.

data Card =

      ReducedAce
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten_Jack_Queen_King
    | Ace
    
    deriving (Eq, Enum, Show)

type CardsShown = ( PlayerCards , DealerCards )

type PlayerCards = [ Card ]

type DealerCards = [ Card ]

type Players = [ Card ]

type ProbabilityOfWinning = Double



--A list of cards that comprise a deck. Note absence of ReducedAce.

deck :: DealerCards
deck = [Two .. Ace]



judgeInitial :: CardsShown -> ProbabilityOfWinning
judgeInitial cards = max doubleCards $ max splitCards $ max ( hit cards ) ( stand cards )

stand :: CardsShown -> ProbabilityOfWinning
stand cards = calcDealer cards

hit = undefined

splitCards = undefined

doubleCards = undefined

--Stuff used to calculate probability of winning after standing, given some CardsShown.

calcDealer :: CardsShown -> ProbabilityOfWinning
calcDealer cardsShown =
  
  sum . map (probabilityOfWinning cardsShown) $
  filterBasedOnCardsShown cardsShown dealerHands



filterBasedOnCardsShown :: (a, [Card]) -> [[Card]] -> [[Card]]
filterBasedOnCardsShown (_ , [dealerFaceUpCard]) = filter (isLastCard dealerFaceUpCard)



isLastCard :: Card -> [Card] -> Bool
isLastCard cardToBeChecked cardsInHand = case cardsInHand of

    [] -> False
    [lastCard] -> cardToBeChecked == lastCard
    (exposedCard : otherCards ) -> isLastCard cardToBeChecked otherCards



collapseCardsShown :: CardsShown -> [Card]
collapseCardsShown ( playerCards , dealerCards ) = playerCards ++ dealerCards



probabilityOfWinning :: CardsShown -> DealerCards -> ProbabilityOfWinning
probabilityOfWinning (collapseCardsShown -> cardsShown) = undefined . reverse



calculateProbabilityOfWinning cardsShown = case of





--Dealer hands aliases for non-Six-Card-Charlie. Also useful for testing.



-- | Generally all dealerHands that don't trigger Six-Card-Charlie.

dealerHandsNotSix :: [DealerCards]
dealerHandsNotSix = filter ( (/= 6 ) . length ) $ dealerHands



-- Specific, individual subsets of dealerHands that don't have Six-Card-Charlie

dealerHands21 :: [DealerCards]
dealerHands21 = filter (valueCheck 21 (==) ) dealerHandsNotSix

dealerHands20 :: [DealerCards]
dealerHands20 = filter (valueCheck 20 (==) ) dealerHandsNotSix

dealerHands19 :: [DealerCards]
dealerHands19 = filter (valueCheck 19 (==) ) dealerHandsNotSix

dealerHands18 :: [DealerCards]
dealerHands18 = filter (valueCheck 18 (==) ) dealerHandsNotSix

dealerHands17 :: [DealerCards]
dealerHands17 = filter (valueCheck 17 (==) ) dealerHandsNotSix



--Dealer Six-Card-Charlie hands

-- | Dealer Six-Card-Charlie hands.

dealerHandsSix :: [DealerCards]
dealerHandsSix = filter ( (==6) . length ) dealerHands



-- | Dealer hands. Iterate creates a list on which elements of the list are repetitions
-- of the action x times, where x is the index of the list starting from 0.

dealerHands :: [DealerCards]
dealerHands = iterate appendNewCard (pure <$> deck) !! 5



-- | Appends a new card, but starts by splitting the existing deck
-- into the parts where a dealer won't hit (under stand on soft 17)
-- and parts where a dealer would hit, "filter valueCheck 17"

appendNewCard :: [[Card]] -> [[Card]]
appendNewCard preExistingDeck =
  
    filter ( valueCheck 17 (<=) ) preExistingDeck

    <>

    ( filter ( valueCheck 17 (>) ) preExistingDeck &

    ( ( ( (:) <$> deck ) <*>) >>> filter ( valueCheck 21 (>=) ) ) )

    


    
-- | Mostly avoids a direct call to cardsToValue, otherwise mostly identical to directly
-- calling the Boolean. Note the invisible [Card] via eta reduction.

valueCheck :: Int -> (Int -> Int -> Bool) -> [Card] -> Bool
valueCheck value boolType = boolType value . cardsToValue 



-- | The next four functions, focusing on cardsToValue, derive the hand value.
-- Note the mutual recursion involved; cardsToValue calls checkForAces, checkForAces
-- calls cardsToValue. This strips aces, one by one, as necessary, to attempt to push
-- the value below 21. If it still fails, checkForAces triggers 22 and breaks out of the
-- recursive loop.
-- Note that ReducedAce never actually appears in a hand, only as an item existing within
-- the card value calculation system.

cardsToValue :: [Card] -> Int
cardsToValue cards =
  
    if basicValueOfCards cards > 21
        then checkForAces cards
        else basicValueOfCards cards
  


-- | Note fromEnum requires (+1) due to the absence of a zero value.

basicValueOfCards :: [Card] -> Int
basicValueOfCards = foldr ( (+) . (+1) . fromEnum ) (0)



-- | As mentioned above, checkForAces is in a mutually-recursive loop with cardsToValue,
-- breaking out via a declaration of 22. It calls reduceAce to reduce aces one by one from
-- 11 to 1 in value.



checkForAces :: [Card] -> Int
checkForAces cards =
  
    if elem Ace cards
        then cardsToValue $ reduceAce cards
        else 22


  
-- | As before, note that reduceAce never actually creates a ReducedAce in a hand, only
-- for cardsToValue, checkForAces, etc, for calculation purposes.

reduceAce :: [Card] -> [Card]
reduceAce hand = case hand of

    [] -> []
    Ace : xs -> ReducedAce:xs
    x : xs -> x : reduceAce xs