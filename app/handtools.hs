{-# LANGUAGE LambdaCase, OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Vector
    ( Vector,
      toList,
      (!),
      filter,
      empty,
      null,
      unfoldrExactN,
      generate,
      cons,
      snoc )
import Types (Card (..))
import Data.Set ( Set, fromAscList, unions, singleton )
import Data.Vector.Generic (modify)
import Data.Vector.Generic.Mutable (write)
import Prelude hiding
    (last, init, map)
import Data.Vector (last)
import Data.Vector (init)
import Data.Set (map)


-- | list of all ranks in Vector form used for combination creation.

twoToAce :: Vector Card
twoToAce =
    [Two .. Ace]

-- | creating all gameStates, within a set, for chart creation.

gameStateList :: Set (Vector Card, Card)
gameStateList =
    appendToLengthNGameState 5 .
    appendToLengthNGameState 4 .
    appendToLengthNGameState 3 $
    appendToLengthNGameState 2
    allPairsAndDealerFaceUps
  where
    allPairsAndDealerFaceUps :: Set (Vector Card, Card)
    allPairsAndDealerFaceUps =
        Data.Set.fromAscList . Data.Vector.toList $
        (generate 2 (const Two) , Two) `cons`
        unfoldrExactN 549 (dupe.addOneToContentsTwoCards)
        (generate 2 (const Two) , Two)

    addOneToContentsTwoCards :: ( Vector Card , Card ) -> ( Vector Card , Card )
    addOneToContentsTwoCards ( vector , dealerFaceUp ) =
        case ( vector Data.Vector.! 0 ,  vector Data.Vector.! 1 ) of
            ( Ace , Ace ) ->
                (
                    generate 2 ( const Two ) ,
                    incrementCardAceUnsafe dealerFaceUp
                )
            ( notAce , Ace ) ->
                (
                    modify
                    (
                        \u -> do
                            write u 0 $ incrementCardAceUnsafe notAce
                            write u 1 $ incrementCardAceUnsafe notAce
                    )
                    vector ,
                    dealerFaceUp
                )
            ( _ , secondElement ) ->
                (
                    modify
                    (
                        \u -> write u 1 $ incrementCardAceUnsafe secondElement
                    )
                    vector ,
                    dealerFaceUp
                )

    appendToLengthNGameState
        :: Int
        -> Set (Vector Card, Card)
        -> Set (Vector Card, Card)
    appendToLengthNGameState givenLength setOfGameStates =
        unions $ map (go givenLength) setOfGameStates
      where
        go :: Int -> (Vector Card, Card) -> Set (Vector Card, Card)
        go givenLength (vectorCard, card)
            | givenLength > length vectorCard =
                singleton (vectorCard, card)
            | otherwise =
                Data.Set.fromAscList . Data.Vector.toList $
                cons (vectorCard , card) $
                    do
                        newCard <- Data.Vector.filter
                            (>= last vectorCard) twoToAce
                        if 21 < cardsValueOf (vectorCard `snoc` newCard)
                            then Data.Vector.empty
                            else pure (vectorCard `snoc` newCard , card)

--A hacky method to increment cards within pseudo-imperative Haskell.

incrementCardAceUnsafe :: Card -> Card
incrementCardAceUnsafe =
    \case
    Two -> Three
    Three -> Four
    Four -> Five
    Five -> Six
    Six -> Seven
    Seven -> Eight
    Eight -> Nine
    Nine -> TenJackQueenKing
    TenJackQueenKing -> Ace
    Ace -> error "attempted to increment an ace"

--Key bit of code, used to calculate the value of a Vector Card.
--Please note that the separate Inner function sems to produce
--better performance, at least on ghci.

cardsValueOf :: Vector Card -> Int
cardsValueOf cardVector =
    cardsValueInner cardVector (0,0)


cardsValueInner :: Vector Card -> (Int,Int) -> Int
cardsValueInner (Data.Vector.null -> True) (aces, otherValue)
        | aces + otherValue > 21 =
            aces + otherValue
        | aces * 11 + otherValue > 21 =
            cardsValueInner Data.Vector.empty (aces-1, otherValue+1)
        | otherwise =
            aces * 11 + otherValue
cardsValueInner cards (aces,otherValue) =
    cardsValueInner (init cards) $
    case last cards of
        Ace ->
            (aces + 1 , otherValue)
        _ ->
            (
                aces,
                (otherValue +) $
                case last cards of
                    Two -> 2
                    Three -> 3
                    Four -> 4
                    Five -> 5
                    Six -> 6
                    Seven -> 7
                    Eight -> 8
                    Nine -> 9
                    TenJackQueenKing -> 10
            )


dupe :: a -> ( a , a )
dupe input = ( input , input )


--Still needs to be checked for accuracy.

--The core dealerHand list, and dealerHands that are checked and summed
--by other mechanisms within the code.


dealerHandList :: Vector (Vector Card) --outstanding problem: 
dealerHandList =
    appendDealerCards .
    appendDealerCards .
    appendDealerCards $
    appendDealerCards
    allPairs
  where
    allPairs :: Vector (Vector Card)
    allPairs =
        generate 2 (const Two) `cons`
        unfoldrExactN 99 (dupe . addOneToContentsTwoCardsDealer)
        (generate 2 (const Two))

    addOneToContentsTwoCardsDealer :: Vector Card -> Vector Card
    addOneToContentsTwoCardsDealer vectorCard =
        case ( vectorCard Data.Vector.! 0 ,  vectorCard Data.Vector.! 1 ) of
            ( notAce , Ace ) ->
                modify
                (
                    \u -> do
                        write u 0 $ incrementCardAceUnsafe notAce
                        write u 1 Two
                )
                vectorCard
            ( _ , secondElement ) ->
                modify
                (\u -> write u 1 $ incrementCardAceUnsafe secondElement)
                vectorCard

    appendDealerCards :: Vector (Vector Card) -> Vector (Vector Card)
    appendDealerCards input =
        do
            oldVector <- input
            if 17 <= cardsValueOf oldVector
                then pure oldVector
                else do
                    newCard <- twoToAce
                    if 21 < cardsValueOf (snoc oldVector newCard)
                        then Data.Vector.empty
                        else pure $ snoc oldVector newCard

--list of dealer hands with six cards, for six card charlie

dealerHandSix :: Vector (Vector Card)
dealerHandSix =
    Data.Vector.filter ((== 6). length) dealerHandList

--dealer hands without 6 cards. Used as a base for a few hands.

dealerHandNotSix :: Vector (Vector Card)
dealerHandNotSix =
    Data.Vector.filter ((/= 6). length) dealerHandList

--various hands that comprise loss conditions.

dealerHand21WithoutNatural :: Vector (Vector Card)
dealerHand21WithoutNatural =
    Data.Vector.filter (`notElem` dealerHandNatural) $
    Data.Vector.filter ( (==21) . cardsValueOf ) dealerHandNotSix


dealerHandNatural :: Vector (Vector Card)
dealerHandNatural =
    [
        [Ace, TenJackQueenKing],
        [TenJackQueenKing,Ace]
    ]


dealerHand21Lose :: Vector (Vector Card)
dealerHand21Lose =
    dealerHandNatural <> dealerHandSix


dealerHand20Lose :: Vector (Vector Card)
dealerHand20Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 20 < cardsValueOf u)
        dealerHandList


dealerHand19Lose :: Vector (Vector Card)
dealerHand19Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 19 < cardsValueOf u)
        dealerHandList


dealerHand18Lose :: Vector (Vector Card)
dealerHand18Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 18 < cardsValueOf u)
        dealerHandList


dealerHand17Lose :: Vector (Vector Card)
dealerHand17Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 17 < cardsValueOf u)
        dealerHandList

--Hands used for calculating ties. Could be used for other things, but for now, it's
--only mentioned in the tie section of the calc EV.

dealerHand17 :: Vector (Vector Card)
dealerHand17 =
    Data.Vector.filter ( (==17) . cardsValueOf ) dealerHandNotSix


dealerHand18 :: Vector (Vector Card)
dealerHand18 =
    Data.Vector.filter ( (==18) . cardsValueOf ) dealerHandNotSix


dealerHand19 :: Vector (Vector Card)
dealerHand19 =
    Data.Vector.filter ( (==19) . cardsValueOf ) dealerHandNotSix


dealerHand20 :: Vector (Vector Card)
dealerHand20 =
    Data.Vector.filter ( (==20) . cardsValueOf ) dealerHandNotSix
