{-# LANGUAGE LambdaCase, PatternSynonyms #-}
module TTI where

import Data.List (intersect, isPrefixOf)
import Control.Applicative ((<**>))
import Data.Function ((&))
import Data.Functor ((<&>))

--Basic types, type synonyms.

data Action =

      Split
    | DoubleAction
    | Hit
    | Stand

    deriving (Eq, Ord)

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

-- Pattern synonym for Ten_Jack_Queen_King added for debugging purposes.

pattern Tens :: Card
pattern Tens = Ten_Jack_Queen_King

type CardsInPlay = ( PlayerCards , DealerCards )

type PlayerCards = [ Card ]

type DealerCards = [ Card ]

type CardsRevealed = [ Card ]

type Players = [ Card ]

type Probability = Double

type ExpectedValue = Double

type Suggestion = (ExpectedValue, Action)



-- | A list of cards that comprise a rank, or all cards of
-- the same suit. Note absence of ReducedAce.

allRanks :: [Card]
allRanks = [ Two .. Ace ]

-- | Hands that are "naturals"

natural :: [[Card]]
natural = [ [ Ace , Ten_Jack_Queen_King ] , [ Ten_Jack_Queen_King , Ace ] ]



judgeInitial :: CardsInPlay -> Probability
judgeInitial cards = maximum [ doubleCards , splitCards , surrender , ( hit cards ) , ( stand cards ) ]
  where
    surrender = 0.50

stand :: CardsInPlay -> Probability
stand cards = calcDealer cards undefined

hit = undefined

splitCards = undefined

doubleCards = undefined

evaluateCards :: CardsInPlay -> Suggestion
evaluateCards cardsInPlay =
  
    max (calculateHits cardsInPlay) (calculateStands cardsInPlay)

calculateHits :: CardsInPlay -> Suggestion
calculateHits cardsInPlay = evaluateCards cardsInPlay

calculateStands :: CardsInPlay -> Suggestion
calculateStands cardsInPlay = ( calculateStandEV cardsInPlay , Stand )

appendNewCardPlayer :: CardsInPlay -> [ CardsInPlay ]
appendNewCardPlayer ( playerCards , dealerFaceUp ) =
  
    fmap (flip (,) dealerFaceUp) . filter (valueCheck 21 (>=)) $
    (pure playerCards) <**> (flip (++) <$> pure <$> allRanks )


returnAnnotatedMax :: [Suggestion] -> Suggestion
returnAnnotatedMax = maximum
  


--Finally have the calculateStandEV calculations partially set up, wherein the

calculateStandEV :: CardsInPlay -> ExpectedValue
calculateStandEV cardsInPlay@( playerCards , dealerFaceUp ) 

{- Basic formula for calculating stand EV is:
  
  probability of tie * 1 +
  probability of loss * 0 +
  probability of win * (win yield)
  
  This is equivalent to, given that:

  probability of win = 1 - probability of tie - probability of loss

  win yield - (probability of tie * win yield) - (probability of loss * winyield)

  Factoring it further, the total calculation comes out to:



  win yield + (probability of tie * (1 - win yield)) - probability of loss * win yield
  
  -}

    | elem playerCards natural =
      
      2.5 - 1.5 * (calcDealer cardsInPlay dealerHandsNatural)

-- Note that naturals take precedence over 6 card charlie.

    | length playerCards == 6, playerValue <- cardsToValue playerCards = error "executeSixCardCharlieLogic"

{- tieCase * 1 + loseCase * 0 + (1-tieCase-loseCase) * 2 for the EV when the player gets 21 value
without 6 card charlie, where x is the odds of tying at 21, y is the
odds of losing to 6 card charlie or naturals. 

Mathematically, this factors out to (2-2tieCase-2loseCase) +x, or 2-2loseCase-tiecase. -}

    | cardsToValue playerCards == 21 =
      
      2 - 2 * calcDealer cardsInPlay dealerHandsSix -
      calcDealer cardsInPlay (dealerHands21 <> dealerHandsNatural)

{- Same as above, except x = dealerHands for 20, y now includes
   dealerHands for 21-}

    | cardsToValue playerCards == 20 =

      2 - 2 *
      (calcDealer cardsInPlay (dealerHandsSix <> dealerHands21)) -
      calcDealer cardsInPlay dealerHands20

{- Same pattern, I ought to just refactor the entire set away into one. -}

    | cardsToValue playerCards == 19 =

      2 - 2 *
      calcDealer cardsInPlay (dealerHandsSix <>
      dealerHands21 <> dealerHands20) -
      calcDealer cardsInPlay dealerHands19

    | cardsToValue playerCards == 18 =

      2 - 2 *
      calcDealer cardsInPlay (dealerHandsSix <> dealerHands21 <>
       dealerHands20 <> dealerHands19) -
      calcDealer cardsInPlay dealerHands18

    | cardsToValue playerCards == 17 =

      2 - 2 *
      calcDealer cardsInPlay (dealerHandsSix <> dealerHands21 <>
      dealerHands20 <> dealerHands19 <> dealerHands18) -
      calcDealer cardsInPlay dealerHands17

    | cardsToValue playerCards < 17 = error "executeBelow17Logic"



calcDealer :: CardsInPlay -> [DealerCards] -> Probability
calcDealer ( playerCards , dealerFaceUp ) =
  
    sum . map (probabilityOfDealerHand playerCards . safeTailThroughNil ) .
    filter (isPrefixOf $ dealerFaceUp )

  where safeTailThroughNil = \case

          [] -> []
          x:xs -> xs


probabilityOfDealerHand :: [Card] -> DealerCards -> Probability
probabilityOfDealerHand cardsInPlay dealerCards = case dealerCards of
    
    [] -> 1
    Ten_Jack_Queen_King:xs -> probabilityTenJackQueenKing cardsInPlay *
        probabilityOfDealerHand (Ten_Jack_Queen_King:cardsInPlay) xs
    otherCard:xs -> probabilityOther cardsInPlay otherCard *
        probabilityOfDealerHand (otherCard:cardsInPlay) xs
        
  where

    numberOfCardsIn specificCard =
      
        fromIntegral . length . intersect [specificCard]

    probabilityTenJackQueenKing cardsInPlay =

      ( 128 - numberOfCardsIn Ten_Jack_Queen_King cardsInPlay ) /
       (416 - fromIntegral (length cardsInPlay) )

    probabilityOther cardsInPlay other =

      ( 32 - numberOfCardsIn other cardsInPlay ) /
       (416 - fromIntegral (length cardsInPlay) )





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

dealerHandsNatural :: [DealerCards]
dealerHandsNatural = natural



--Dealer Six-Card-Charlie hands

-- | Dealer Six-Card-Charlie hands.

dealerHandsSix :: [DealerCards]
dealerHandsSix = filter ( (==6) . length ) dealerHands



-- | Dealer hands. Iterate creates a list on which elements of the list are repetitions
-- of the action x times, where x is the index of the list starting from 0.

dealerHands :: [DealerCards]
dealerHands = iterate appendNewCardDealer (pure <$> allRanks) !! 5



-- | Appends a new card to a set of cards, but starts by splitting
-- the existing deck into the parts where a dealer won't hit
-- (under stand on soft 17) and parts where a dealer would hit,
-- "filter valueCheck 17"

appendNewCardDealer :: [[Card]] -> [[Card]]
appendNewCardDealer preExistingCards =

    let dealerStandCards = filter ( valueCheck 17 (<=) ) preExistingCards
        dealerHitCards =
          
          filter ( valueCheck 21 (>=) ) $
          ( filter ( valueCheck 17 (>) ) preExistingCards ) <**>
          (flip (++) <$> pure <$> allRanks) in

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