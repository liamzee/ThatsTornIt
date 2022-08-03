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

--A list of cards that comprise a deck. Note absence of ReducedAce.

deck :: DealerCards
deck = [Two .. Ace]



judgeInitial :: Ord a => CardsShown -> a
judgeInitial cards = max doubleCards $ max splitCards $ max ( hit cards ) ( stand cards )

stand cards = calcDealer cards

calcDealer cardsShown = undefined

hit = undefined

splitCards = undefined

doubleCards = undefined



--Dealer hands aliases for non-Six-card-Charlie



dealerHands21 :: [DealerCards]
dealerHands21 = filter (valueCheck 21 (==) ) . filter ( (/= 6) . length ) $ dealerHands

dealerHands20 :: [DealerCards]
dealerHands20 = filter (valueCheck 20 (==) ) . filter ( (/= 6) . length ) $ dealerHands

dealerHands19 :: [DealerCards]
dealerHands19 = filter (valueCheck 19 (==) ) . filter ( (/= 6) . length ) $ dealerHands

dealerHands18 :: [DealerCards]
dealerHands18 = filter (valueCheck 18 (==) ) . filter ( (/= 6) . length ) $ dealerHands

dealerHands17 :: [DealerCards]
dealerHands17 = filter (valueCheck 17 (==) ) . filter ( (/= 6) . length ) $ dealerHands



--Dealer six card charlie hands



dealerHandsSix :: [DealerCards]
dealerHandsSix = filter ( (==6) . length ) dealerHands



--Dealer hands



dealerHands :: [DealerCards]
dealerHands =
  
  pure <$> deck & ( appendNewCard >>> appendNewCard >>>
  appendNewCard >>> appendNewCard >>> appendNewCard )

-- | Obviously appends a new card, but starts by splitting the existing deck
-- into the parts where a dealer won't hit (under stand on soft 17)
-- and parts where a dealer would hit, "filter valueCheck 17"

appendNewCard :: [[Card]] -> [[Card]]
appendNewCard preExistingDeck =
  
    filter ( valueCheck 17 (<=) ) preExistingDeck

    <>

    ( filter ( valueCheck 17 (>) ) preExistingDeck &

    ( ( ( (:) <$> deck ) <*>) >>> filter ( valueCheck 21 (>=) ) ) )

    


    
--Note that the current reduced-point implementation of valueCheck
--puts the value to be compared to on the left. 

valueCheck :: Int -> (Int -> Int -> Bool) -> [Card] -> Bool
valueCheck value boolType = boolType value . cardsToValue 



cardsToValue :: [Card] -> Int
cardsToValue cards = if basicValueOfCards cards > 21
  then checkForAces cards
  else basicValueOfCards cards
  


basicValueOfCards :: [Card] -> Int
basicValueOfCards = foldr ( (+) . (+1) . fromEnum ) (0)



checkForAces :: [Card] -> Int
checkForAces cards = if elem Ace cards
  then cardsToValue $ reduceAce cards
  else 22



reduceAce :: [Card] -> [Card]
reduceAce [] = []
reduceAce (Ace:xs) = ReducedAce:xs
reduceAce (x:xs) = x : reduceAce xs
