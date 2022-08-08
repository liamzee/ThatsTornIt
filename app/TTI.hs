{-# LANGUAGE LambdaCase, PatternSynonyms #-}
module TTI where

import Data.List (intersect, isPrefixOf, (\\))
import Control.Applicative ((<**>))

--Basic types, type synonyms.

data Action =

      Split
    | DoubleAction
    | Hit
    | Stand

    deriving (Eq, Ord, Show)

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



-- Some safe versions of list functions, written with nil as a default value


safeTailThroughNil :: [a] -> [a]
safeTailThroughNil = \case

    [] -> []
    x:xs -> xs



safeInitThroughNil :: [a] -> [a]
safeInitThroughNil = \case

    [] -> []
    [x] -> []
    x:xs -> x : safeInitThroughNil xs



safeLastThroughNil :: [a] -> [a]
safeLastThroughNil = \case

    [] -> []
    [x] -> [x]
    x:xs -> safeLastThroughNil xs



  -- Used both in dealer and player sides, easier to move this out of a where block.





numberOfCardsIn :: Card -> [Card] -> Double
numberOfCardsIn specificCard =
      
    fromIntegral . length . intersect [specificCard]



probabilityTenJackQueenKing :: [Card] -> Double
probabilityTenJackQueenKing cardsInPlay =

    ( 128 - numberOfCardsIn Ten_Jack_Queen_King cardsInPlay ) /
    (416 - fromIntegral (length cardsInPlay) )



probabilityOther :: [Card] -> Card -> Double
probabilityOther cardsInPlay other =

    ( 32 - numberOfCardsIn other cardsInPlay ) /
    (416 - fromIntegral (length cardsInPlay) )


{-
judgeInitial :: CardsInPlay -> Probability
judgeInitial cards = maximum [ doubleCards , splitCards , surrender , ( hit cards ) , ( stand cards ) ]
  where
    surrender = 0.50

stand :: CardsInPlay -> Probability
stand cards = calcDealer cards undefined

hit = undefined

splitCards = undefined

doubleCards = undefined-}


{- Anna Kournikova! It looks good, but it's absolutely broken. I'm getting 90 minute computations on a single
version, with an 4900 EV, and 1.4 TB of RAM used. -}

evaluateHitVsStand :: CardsInPlay -> Suggestion
evaluateHitVsStand cardsInPlay =
  
    evaluateHits (calculateStands cardsInPlay) $
    evaluateHitVsStand <$> appendNewCardPlayer cardsInPlay



evaluateHits :: Suggestion -> [Suggestion] -> Suggestion
evaluateHits stand [] = stand
evaluateHits stand hit = max stand (sum $ fst <$> hit, Hit)



calculateStands :: CardsInPlay -> Suggestion
calculateStands cardsInPlay = 
  
    ( probabilityOfPlayerDraw cardsInPlay *
    calculateStandEV cardsInPlay ,
     Stand )



appendNewCardPlayer :: CardsInPlay -> [ CardsInPlay ]
appendNewCardPlayer cardsInPlay@( playerCards , dealerFaceUp ) =
  
    fmap (flip (,) dealerFaceUp) . filter (valueCheck 21 (>=)) $
    (pure playerCards) <**> (flip (++) <$> pure <$> allRanks )



probabilityOfPlayerDraw :: CardsInPlay -> Probability
probabilityOfPlayerDraw cardsInPlay@(playerCards, dealerCards) =
  
    let combinedCardsInPlay = safeInitThroughNil playerCards ++ dealerCards in
    
    case safeLastThroughNil playerCards of

        [] -> 1
        [Ten_Jack_Queen_King] -> probabilityTenJackQueenKing combinedCardsInPlay
        [other] -> probabilityOther combinedCardsInPlay other



{- Finally have the calculateStandEV calculations partially set up. This overall
section is mostly complete, except for the calculateStandEV calculations.-}



calculateStandEV :: CardsInPlay -> ExpectedValue
calculateStandEV cardsInPlay@( playerCards , dealerFaceUp ) 

{- Refactoring, there's only three cases that matter. First,
there's the case of 6 card charlie. Then the player wins provided
that the dealer doesn't have a natural, and that the opponent doesn't
have a 6 card charlie of their own.

Second, there's the case of a player natural. The player always wins
unless the opponent has naturals, in which case there's a tie.

Third, there's the case of player 21. Uniquely here, the player loses to a natural,
and ties to a dealer 21.

Fourth, there's any other case.

There's also the basic calculation of wins, loss, and ties. In the case of loss,
since we set EV to 1 on tie, we won't calculate anything. In the case of winning,
we'll set the yield to 2, unless it's a natural, in which case the yield is 2.5.

The general formula then comes out to:

1 * probability of tie + 2 * probability of winning
probability of winning = 1 - probability of tie - probability of losing

then:

probability of tie + (2 * (1 - probability of tie - probability of losing)

2 + (-2 * probability of tie) - 2 * probability of losing + probability of tie

2 - 2 * probability of losing - probability of tie.

For naturals:

2.5 - 1.5 * probability of tie

-}

    | length playerCards == 6 =

        2 - 2 * calcDealer cardsInPlay
        
        (

        dealerHandsNatural <>
        (filter (valueCheck playerHandValue (<)) dealerHandsSix)

        )

        - calcDealer cardsInPlay
        (filter (valueCheck playerHandValue (==)) dealerHandsSix)

    | elem playerCards natural =
    2.5 - 1.5 * calcDealer cardsInPlay dealerHandsNatural

    | playerHandValue == 21 =

        2 - 2 * calcDealer cardsInPlay
        
        (

        dealerHandsSix <>
        dealerHandsNatural

        )

        - calcDealer cardsInPlay
        ( dealerHands21 \\ dealerHandsNatural )

    | otherwise = 

          2 - 2 * calcDealer cardsInPlay
      
          (

          dealerHandsSix <>
          filter (valueCheck playerHandValue (<)) dealerHandsNotSix

          )

          - calcDealer cardsInPlay
          (filter (valueCheck playerHandValue (==)) dealerHandsNotSix)


  where

    playerHandValue = cardsToValue playerCards

calcDealer :: CardsInPlay -> [DealerCards] -> Probability
calcDealer ( playerCards , dealerFaceUp ) =
  
    sum . map (probabilityOfDealerHand playerCards . safeTailThroughNil ) .
    filter (isPrefixOf $ dealerFaceUp )



probabilityOfDealerHand :: [Card] -> DealerCards -> Probability
probabilityOfDealerHand cardsInPlay dealerCards = case dealerCards of
    
    [] -> 1
    Ten_Jack_Queen_King:xs -> probabilityTenJackQueenKing cardsInPlay *
        probabilityOfDealerHand (Ten_Jack_Queen_King:cardsInPlay) xs
    otherCard:xs -> probabilityOther cardsInPlay otherCard *
        probabilityOfDealerHand (otherCard:cardsInPlay) xs





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



{- The subsequent functions generate the term "dealerHands", which contains all possible dealer hands. -}



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