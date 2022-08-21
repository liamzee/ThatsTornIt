{-# LANGUAGE OverloadedStrings, LambdaCase, MonadComprehensions #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}

{-This file, at least for now, is going to be done as a single module.
Comments like these will split up the parts of the module, it's not good
design, but I've lost my confidence in the inliner.

The existence of shared functions also makes it somewhat harder
to understand the organization of program, which probably
led to substantial bugs with previous iterations.-}

module Main where

import Data.Time (getCurrentTime)
import Data.Aeson ( FromJSON, ToJSON, encode )
import qualified Data.ByteString.Lazy as LB
import GHC.Generics (Generic)
import Graphics.UI.TinyFileDialogs ( saveFileDialog )
import Data.Text ( Text, unpack )
import Data.Maybe ( fromMaybe )
import Data.Vector.Generic.Mutable (write)
import Data.Vector ( Vector, generate, (!), modify,
    empty, cons, snoc, last,
    null, splitAt, init, length,
    sum, tail, filter, unfoldrExactN,
    toList, (//))
import Prelude hiding (map, sum, null, length, last,
    splitAt, init, tail, filter)
import Criterion.Main
import Control.DeepSeq ( NFData, force )
import Control.Parallel.Strategies
import Control.Monad ((<=<), (>=>), join)
import Data.Map.Strict ( Map, fromSet )
import Data.Set


data Card
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | TenJackQueenKing
    | Ace

    deriving (Show, Generic, Eq, Ord, Enum, NFData)

instance FromJSON Card
instance ToJSON Card


data Action
    = Stand
    | Hit
    | Surrender
    | DoubleAction
    | Split

    deriving (Show, Generic, Eq, Ord, NFData )

instance FromJSON Action
instance ToJSON Action


data AllowedActions
    = ActionsSplitSurrenderDouble
    | ActionsSurrenderDouble
    | ActionsDouble
    | HitStandOnly

    deriving (Show, Generic, Eq, Ord, NFData )

instance FromJSON AllowedActions
instance ToJSON AllowedActions


newtype GameState
    = GameState

    {
      gameState :: ( PlayerCards , DealerFaceUp )
    }

    deriving (Show, Generic , Eq , Ord, NFData )

instance FromJSON GameState
instance ToJSON GameState


newtype BlackjackActionDirectoryTopLevel
    = BlackjackActionDirectoryTopLevel

    {
      mainBranches :: Vector ( GameState , BranchContents )
    }

    deriving (Show, Generic, NFData)

instance FromJSON BlackjackActionDirectoryTopLevel
instance ToJSON BlackjackActionDirectoryTopLevel


type DealerFaceUp = Card
type PlayerCards = Vector Card
type DealerHand = Vector Card


newtype BranchContents
    = BranchContents

    {
      branchContents :: Vector ( GameState , AnnotatedSuggestions )
    }

    deriving ( Show , Generic , Eq , Ord, NFData )

instance FromJSON BranchContents
instance ToJSON BranchContents


newtype AnnotatedSuggestions
    = AnnotatedSuggestions

    {
      annotatedSuggestions :: Vector ( AllowedActions , Suggestion , Probability )
    }

    deriving ( Show , Generic , Eq , Ord , NFData )

instance FromJSON AnnotatedSuggestions
instance ToJSON AnnotatedSuggestions


type Probability = () -- not providing this functionality right now.
type EV = Double

newtype Suggestion
    = Suggestion

    {
      suggestion :: ( EV , Action )
    }

    deriving ( Show , Generic , Eq , Ord , NFData)

instance FromJSON Suggestion
instance ToJSON Suggestion

--Rebuilt around 3 main memoizations, namely the
--standEV list, the player hands list, and
--the dealer hands list.

main :: IO ()
main = undefined


-- | list of all ranks in Vector form used for combination creation.

twoToAce :: Vector Card
twoToAce =
    pure Two `snoc`
    Three `snoc`
    Four `snoc`
    Five `snoc`
    Six `snoc`
    Seven `snoc`
    Eight `snoc`
    Nine `snoc`
    TenJackQueenKing `snoc`
    Ace


gameStateList :: Set (Vector Card, Card)
gameStateList =
    appendToLengthNGameState 4 $
    appendToLengthNGameState 3 $
    appendToLengthNGameState 2 $
    allPairsAndDealerFaceUps
  where
    allPairsAndDealerFaceUps :: Set (Vector Card, Card)
    allPairsAndDealerFaceUps =
        fromAscList . Data.Vector.toList $
        (generate 2 (const Two) , Two) `cons`
        unfoldrExactN 549(dupe.addOneToContentsTwoCards)
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
                fromList . Data.Vector.toList $
                cons (vectorCard , card) $
                    do
                        newCard <- Data.Vector.filter
                            (>= last vectorCard) twoToAce
                        if 21 < cardsValueOf (vectorCard `snoc` newCard)
                            then Data.Vector.empty
                            else pure (vectorCard `snoc` newCard , card)



incrementCardAceUnsafe :: Card -> Card
incrementCardAceUnsafe = \case
    Two -> Three
    Three -> Four
    Four -> Five
    Five -> Six
    Six -> Seven
    Seven -> Eight
    Eight -> Nine
    Nine -> TenJackQueenKing
    TenJackQueenKing -> Ace


cardsValueOf :: Vector Card -> Int
cardsValueOf cardVector =
    cardsValueInner cardVector 0 0

cardsValueInner :: Vector Card -> Int -> Int -> Int
cardsValueInner (Data.Vector.null -> True) aces otherValue
        | aces + otherValue > 21 =
            aces + otherValue
        | aces * 11 + otherValue > 21 =
            cardsValueInner Data.Vector.empty (aces-1) (otherValue+1)
        | otherwise =
            aces * 11 + otherValue
cardsValueInner cards aces otherValue =
        case last cards of
            Ace ->
                cardsValueInner (init cards) ( aces + 1 ) otherValue
            _ ->
                cardsValueInner (init cards) aces $
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


dupe :: a -> ( a , a )
dupe input = ( input , input )
            

dealerHandList :: Vector (Vector Card) --outstanding problem: 
dealerHandList =
    appendDealerCards $
    appendDealerCards $
    appendDealerCards $
    appendDealerCards $
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
                        write u 1 $ Two
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


dealerHandSix :: Vector (Vector Card)
dealerHandSix =
    Data.Vector.filter ((== 6). length) dealerHandList

dealerHandNotSix :: Vector (Vector Card)
dealerHandNotSix =
    Data.Vector.filter ((/= 6). length) dealerHandList


dealerHand21WithoutNatural :: Vector (Vector Card)
dealerHand21WithoutNatural =
    Data.Vector.filter (flip notElem dealerHandNatural) $
    Data.Vector.filter ( (==21) . cardsValueOf ) dealerHandNotSix
                                                 

dealerHandNatural :: Vector (Vector Card)
dealerHandNatural =
    pure (pure Ace `snoc` TenJackQueenKing) `snoc`
    (pure TenJackQueenKing `snoc` Ace)


dealerHand21Lose :: Vector (Vector Card)
dealerHand21Lose =
    Data.Vector.filter
        (\u ->
        (==6) (length u) ||
        elem u 
            [
                (pure Ace `snoc` TenJackQueenKing),
                (pure TenJackQueenKing `snoc` Ace)
            ]
        )
        dealerHandList


dealerHand20Lose :: Vector (Vector Card)
dealerHand20Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 21 >= cardsValueOf u)
        dealerHandList


dealerHand19Lose :: Vector (Vector Card)
dealerHand19Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 20 >= cardsValueOf u)
        dealerHandList


dealerHand18Lose :: Vector (Vector Card)
dealerHand18Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 19 >= cardsValueOf u)
        dealerHandList


dealerHand17Lose :: Vector (Vector Card)
dealerHand17Lose =
    Data.Vector.filter
        (\u -> (==6) (length u)  || 18 >= cardsValueOf u)
        dealerHandList


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


standEVMap :: Map (Vector Card, Card) EV
standEVMap = fromSet calculateStandEV standEVSet
  where
    standEVSet :: Set (Vector Card, Card)
    standEVSet = gameStateList


    --Just working it out, the EV of a stand action should be:
    --
    --probabilityOfTie * tieValue + probabilityOfLoss * lossValue +
    --probabilityOfWin * winValue
    --
    --probabilityOfWin = 1 - probabilityOfTie - probabilityOfLoss
    --
    --then we can simplify to, by setting lossValue to 0, allowing us to ignore
    --it, tieValue to 1, and winValue to 2
    --
    --probabilityOfTie + (1 - probabilityOfTie - probabilityOfLoss) * 2
    --
    --2 - (2 * probabilityOfLoss) - probabilityOfTie
    --
    --In the natural case, we have 2.5 - 2.5 * probabilityOfLoss
    -- - 2.5 * probabilityOfTie  + 1 probabilityOfTie
    --
    --We cannot lose with a natural, so
    --2.5 - 1.5 * probabilityOfTie

    calculateStandEV :: (Vector Card, Card) -> EV
    calculateStandEV boardPosition@(playerHand, dealerFaceUp)
        | length playerHand == 2,
          playerHand `elem`
          [pure Ace `snoc` TenJackQueenKing, pure TenJackQueenKing `snoc` Ace] =
            2.5 -
            (
                1.5 *
                calculateProbabilityFromDealerHands boardPosition
                dealerHandNatural
            )
        | length playerHand == 6,
          playerHandValue <- cardsValueOf playerHand =
            twoWinMinus2LossMinusTie boardPosition
                (
                    dealerHandNatural <>
                    Data.Vector.filter
                    (
                        ( playerHandValue < ) .
                        cardsValueOf
                    )
                    dealerHandSix
                )
                (
                Data.Vector.filter
                (
                    ( playerHandValue == ) .
                    cardsValueOf
                )
                dealerHandSix
                )
        | cardsValueOf playerHand == 21 =
            twoWinMinus2LossMinusTie boardPosition
                dealerHand21Lose
                dealerHand21WithoutNatural
        | cardsValueOf playerHand == 20 =
            twoWinMinus2LossMinusTie boardPosition
                dealerHand20Lose
                dealerHand20
        | cardsValueOf playerHand == 19 =
            twoWinMinus2LossMinusTie boardPosition
                dealerHand19Lose
                dealerHand19
        | cardsValueOf playerHand == 18 =
            twoWinMinus2LossMinusTie boardPosition
                dealerHand18Lose
                dealerHand18
        | cardsValueOf playerHand == 17 =
            twoWinMinus2LossMinusTie boardPosition
                dealerHand17Lose
                dealerHand17
        | otherwise =
            twoWinMinus2LossMinusTie boardPosition
                dealerHandList
                Data.Vector.empty
      where
        twoWinMinus2LossMinusTie
            :: (Vector Card, Card)
            -> Vector (Vector Card)
            -> Vector (Vector Card)
            -> EV
        twoWinMinus2LossMinusTie boardPosition lossPositions tiePositions =
            2 -
            (
                2 *
                (
                    calculateProbabilityFromDealerHands
                        boardPosition
                        lossPositions
                )
            )
            -
            (
                calculateProbabilityFromDealerHands
                    boardPosition
                    tiePositions
            )

    calculateProbabilityFromDealerHands
        :: (Vector Card, Card) -> Vector (Vector Card) -> EV
    calculateProbabilityFromDealerHands boardPosition dealerHands =
        sum $
        calculateIndividualDealerHandProbability boardPosition <$>
        dealerHands

    calculateIndividualDealerHandProbability
        :: (Vector Card, Card) -> Vector Card -> EV
    calculateIndividualDealerHandProbability boardPosition specificDealerHand =
        1 --holding value so that benchmarks for the structure can be done.