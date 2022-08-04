{-# LANGUAGE ViewPatterns #-}
module TTI where

import Control.Category hiding ((.))
import Data.Function ((&))
import Data.List (delete)

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

type CardsInPlay = ( PlayerCards , DealerCards )

type PlayerCards = [ Card ]

type DealerCards = [ Card ]

type CardsRevealed = [ Card ]

type Players = [ Card ]

type ProbabilityOfWinning = Double



--A list of cards that comprise a deck. Note absence of ReducedAce.

deck :: DealerCards
deck = [Two .. Ace]



judgeInitial :: CardsInPlay -> ProbabilityOfWinning
judgeInitial cards = max doubleCards $ max splitCards $ max ( hit cards ) ( stand cards )

stand :: CardsInPlay -> ProbabilityOfWinning
stand cards = calcDealer cards undefined

hit = undefined

splitCards = undefined

doubleCards = undefined



--Stuff used to calculate probability of winning after standing,
--given some cardsInPlay.

--Currently, very bad bugs using pointsChooser and Natural vs Ace / King as test pieces.
--Same occurs with calculateProbabilityOfWinning.

pointsChooser :: CardsInPlay -> ProbabilityOfWinning
pointsChooser cardsInPlay@( playerCards , dealerFaceUp ) 

    | length playerCards == 6 = error "executeSixCardCharlieLogic"
    | elem playerCards [ [Ten_Jack_Queen_King, Ace] , [Ace, Ten_Jack_Queen_King] ]
      = calcDealer cardsInPlay [ playerCards , reverse playerCards ]
    | cardsToValue playerCards == 21 = error "execute21ValueLogic"
    | cardsToValue playerCards == 20 = error "execute20ValueLogic"
    | cardsToValue playerCards == 19 = error "execute19ValueLogic"
    | cardsToValue playerCards == 18 = error "execute18ValueLogic"
    | cardsToValue playerCards == 17 = error "execute17ValueLogic"
    | cardsToValue playerCards < 17 = error "executeBelow17Logic"



calcDealer :: CardsInPlay -> [DealerCards] -> ProbabilityOfWinning
calcDealer cardsInPlay =
  
  sum . map (probabilityOfWinning cardsInPlay) .
  filterBasedOnCardsShown cardsInPlay



filterBasedOnCardsShown :: ( a , [ Card ] ) -> [ [ Card ] ] -> [ [ Card ] ]
filterBasedOnCardsShown (_ , [dealerFaceUpCard]) =
  
    filter (isLastCard dealerFaceUpCard)



isLastCard :: Card -> [Card] -> Bool
isLastCard cardToBeChecked cardsInHand = case cardsInHand of

    [] -> False
    [lastCard] -> cardToBeChecked == lastCard
    (exposedCard : otherCards ) -> isLastCard cardToBeChecked otherCards



collapseCardsInPlay :: CardsInPlay -> [Card]
collapseCardsInPlay ( playerCards , dealerCards ) = playerCards <> dealerCards



probabilityOfWinning :: CardsInPlay -> DealerCards -> ProbabilityOfWinning
probabilityOfWinning (collapseCardsInPlay -> cardsShown) =
  
    calculateProbabilityOfWinning cardsShown . reverse



calculateProbabilityOfWinning :: [Card] -> DealerCards -> ProbabilityOfWinning
calculateProbabilityOfWinning cardsShown dealerCards = case dealerCards of
    
    [] -> 1
    Ten_Jack_Queen_King:xs -> probabilityTenJackQueenKing cardsShown *
        calculateProbabilityOfWinning (Ten_Jack_Queen_King:cardsShown) xs
    other:xs -> probabilityOther cardsShown other *
        calculateProbabilityOfWinning (other:cardsShown) xs
        
  where

    numberOfCardsIn cardsShown specificCard =
        
        fromIntegral . length . filter (== specificCard) $ cardsShown

    probabilityTenJackQueenKing cardsShown =
      ( 128 - numberOfCardsIn cardsShown Ten_Jack_Queen_King ) / 416

    probabilityOther cardsShown other =
      ( 32 - numberOfCardsIn cardsShown other ) / 416





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



-- | Appends a new card to a set of cards, but starts by splitting
-- the existing deck into the parts where a dealer won't hit
-- (under stand on soft 17) and parts where a dealer would hit,
-- "filter valueCheck 17"

appendNewCard :: [[Card]] -> [[Card]]
appendNewCard preExistingCards =

    let dealerStandCards = filter ( valueCheck 17 (<=) ) preExistingCards
        dealerHitCards =
          
          ( filter ( valueCheck 21 (>=) ) $ ( (:) <$> deck ) <*>
          filter ( valueCheck 17 (>) ) preExistingCards ) in

          --Last value filters preExistingCards based on player hitting,
          --starting at the end. The ((:) <$> deck) <*> adds cards, then
          --the filter before it removes combinations that would cause the
          --dealer to go bust.

    dealerStandCards <> dealerHitCards

    


    
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