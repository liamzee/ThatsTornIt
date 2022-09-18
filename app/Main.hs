{-# LANGUAGE OverloadedStrings, LambdaCase, PatternSynonyms #-}
{-# LANGUAGE OverloadedLists, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns, TupleSections, ApplicativeDo #-}

{-This file, at least for now, is going to be done as a single module.
Comments like these will split up the parts of the module, it's not good
design, but I've lost my confidence in the inliner.

The existence of shared functions also makes it somewhat harder
to understand the organization of program, which probably
led to substantial bugs with previous iterations.-}

{-
    So far, there are a few key painpoints that need to be verified.
    First, are the various probability calculators correct?
    Second, are the generators for cards correct?
-}

{- 

STANDING AND IMPORTANT QUESTION:

DOES MY ALGORITHM ACCOUNT FOR PLAYER BUST CASES?

-}

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
    toList, (//), head, elem, thaw, freeze, fromList, partition, maximum,
    fromList)
import Prelude hiding (head, map, sum, null, length, last,
    splitAt, init, tail, filter)
import Criterion.Main ()
import Control.DeepSeq ( NFData, force, deepseq )
import Control.Parallel.Strategies ( NFData, rpar, rseq, runEval )
import Control.Monad ((<=<), (>=>), join)
import Data.Map.Lazy ( Map, fromSet, union, (!), fromList, foldl' )
import Data.Set
    ( filter,
      fromAscList,
      fromList,
      map,
      singleton,
      size,
      splitAt,
      toList,
      unions,
      Set )
import Control.Monad.ST (runST)
import Data.List (sort, intersect, null)
import Debug.Trace (trace, traceShowId)
import Control.Arrow ( Arrow(first, second) )
import Data.Functor ((<&>))
import Data.Bifunctor (bimap)
import Data.Map (fold)
import ProbabilityCalculator (probabilityOfEventCalculator)
import Types
import qualified CalculateStand
import qualified CalculateTypes as CT
import CalculateStand (calculateStand, mapStandEV)
import qualified Parallelize
import Data.Coerce (coerce)
import Unsafe.Coerce
import EvaluateActions (evaluateHitStand, calculateDouble, calculateSplit, evaluateSplitDoubleSurrender)
import TotalEVTester (checkEVofGame)
import MakeAllValidBoardStatesSplit (realSnuff)
import CalculateNonSplitBoardStates (allNonSplitBoardStates)


pattern Tens :: CT.Card
pattern Tens = CT.TenJackQueenKing

--Rebuilt around 3 main memoizations, namely the
--standEV list, the player hands list, and
--the dealer hands list.

main :: IO ()
main = do
--    print TotalEVTester.checkEVofGame
    tfd <- saveFileDialog "" "" [""] "" <&> (unpack . fromMaybe "")
    writeJSONOutput tfd

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
    Data.Vector.filter (`Prelude.notElem` dealerHandNatural) $
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

-- A map that memoizes all standEV results up to length 6, replacing
-- calculations with lookups for hit vs stand calculations and recommendations
-- for actions.

parallelize :: NFData a => (BoardPosition -> a) -> Map BoardPosition a
parallelize conversionFunction =

    runEval $ do
        let (set1, set2) =
                Data.Set.splitAt (div (size gameStateList) 2) gameStateList
        let (set11, set12) = Data.Set.splitAt (div (size set1) 2) set1
        let (set21, set22) = Data.Set.splitAt (div (size set2) 2) set2
        let (set111, set112) = Data.Set.splitAt (div (size set11) 2) set11
        let (set121, set122) = Data.Set.splitAt (div (size set12) 2) set12
        let (set211, set212) = Data.Set.splitAt (div (size set21) 2) set21
        let (set221, set222) = Data.Set.splitAt (div (size set22) 2) set22
        let (set1111, set1112) = Data.Set.splitAt (div (size set22) 2) set111
        let (set1121, set1122) = Data.Set.splitAt (div (size set22) 2) set112
        let (set1211, set1212) = Data.Set.splitAt (div (size set22) 2) set121
        let (set1221, set1222) = Data.Set.splitAt (div (size set22) 2) set122
        let (set2111, set2112) = Data.Set.splitAt (div (size set22) 2) set211
        let (set2121, set2122) = Data.Set.splitAt (div (size set22) 2) set212
        let (set2211, set2212) = Data.Set.splitAt (div (size set22) 2) set221
        let (set2221, set2222) = Data.Set.splitAt (div (size set22) 2) set222

        map1111 <- rpar $ force $ fromSet conversionFunction set1111
        map1112 <- rpar $ force $ fromSet conversionFunction set1112
        map1121 <- rpar $ force $ fromSet conversionFunction set1121
        map1122 <- rpar $ force $ fromSet conversionFunction set1122

        map1211 <- rpar $ force $ fromSet conversionFunction set1211
        map1212 <- rpar $ force $ fromSet conversionFunction set1212
        map1221 <- rpar $ force $ fromSet conversionFunction set1221
        map1222 <- rpar $ force $ fromSet conversionFunction set1222

        map2111 <- rpar $ force $ fromSet conversionFunction set2111
        map2112 <- rpar $ force $ fromSet conversionFunction set2112
        map2121 <- rpar $ force $ fromSet conversionFunction set2121
        map2122 <- rpar $ force $ fromSet conversionFunction set2122

        map2211 <- rpar $ force $ fromSet conversionFunction set2211
        map2212 <- rpar $ force $ fromSet conversionFunction set2212
        map2221 <- rpar $ force $ fromSet conversionFunction set2221
        map2222 <- rpar $ force $ fromSet conversionFunction set2222


        rseq map1111 >> rseq map1112 >> rseq map1121 >> rseq map1122
        rseq map1211 >> rseq map1212 >> rseq map1221 >> rseq map1222
        rseq map2111 >> rseq map2112 >> rseq map2121 >> rseq map2122
        rseq map2211 >> rseq map2212 >> rseq map2221 >> rseq map2222
        pure $ (((map1111 `union` map1112) `union` (map1121 `union` map1122)) `union`
            (map1211 `union` map1212) `union` (map1221 `union` map1222)) `union`
            (((map2111 `union` map2112) `union` (map2121 `union` map2122)) `union`
            ((map2211 `union` map2212) `union` (map2221 `union` map2222)))

sumDealerProbabilityMap2 :: EV
sumDealerProbabilityMap2 = foldl' (+) 0 dealerProbabilityMap2

dealerProbabilityMap2 :: Map (Vector Card) EV
dealerProbabilityMap2 = Data.Map.Lazy.fromSet checkProbability (Data.Set.fromList . Data.Vector.toList $ dealerHandList)
  where
    checkProbability :: Vector Card -> EV
    checkProbability specificDealerHand = calculateIndividualDealerHandProbability specificDealerHand
      where
        calculateIndividualDealerHandProbability
            :: Vector Card -> EV
        calculateIndividualDealerHandProbability specificDealerHand =
            go (pure $ head specificDealerHand) (tail specificDealerHand) 1
          where
            go :: Vector Card -> Vector Card -> Double -> EV
            go _ (Data.Vector.null -> True) storedProbability =
                storedProbability
            go cardsInPlay dealerHand@(Data.Vector.head -> TenJackQueenKing)
                storedProbability =
                    go (cardsInPlay `snoc` TenJackQueenKing) (tail dealerHand)
                    (tensCalc cardsInPlay * storedProbability)
            go cardsInPlay dealerHand storedProbability =
                let other = Data.Vector.head dealerHand in

                go (cardsInPlay `snoc` other) (tail dealerHand)
                (otherCalc cardsInPlay other * storedProbability)


        tensCalc :: Vector Card -> EV
        tensCalc cardsInPlay =
            fromIntegral
            (
                126 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay )
            )
            /
            fromIntegral
            (414 - length cardsInPlay )


        otherCalc :: Vector Card -> Card -> EV
        otherCalc cardsInPlay card =
            fromIntegral
            (
                30 -
                length ( Data.Vector.filter (==card) cardsInPlay )
            )
            /
            fromIntegral
            (414 - length cardsInPlay)


dealerProbabilityMap3 :: Map (Vector Card) EV
dealerProbabilityMap3 = Data.Map.Lazy.fromSet checkProbability (Data.Set.fromList . Data.Vector.toList $ dealerHandList)
  where
    checkProbability :: Vector Card -> EV
    checkProbability specificDealerHand = calculateIndividualDealerHandProbability specificDealerHand
      where
        calculateIndividualDealerHandProbability
            :: Vector Card -> EV
        calculateIndividualDealerHandProbability specificDealerHand =
            go (pure $ head specificDealerHand) (tail specificDealerHand) 1
          where
            go :: Vector Card -> Vector Card -> Double -> EV
            go _ (Data.Vector.null -> True) storedProbability =
                storedProbability
            go cardsInPlay dealerHand@(Data.Vector.head -> TenJackQueenKing)
                storedProbability =
                    go (cardsInPlay `snoc` TenJackQueenKing) (tail dealerHand)
                    (tensCalc cardsInPlay * storedProbability)
            go cardsInPlay dealerHand storedProbability =
                let other = Data.Vector.head dealerHand in

                go (cardsInPlay `snoc` other) (tail dealerHand)
                (otherCalc cardsInPlay other * storedProbability)


        tensCalc :: Vector Card -> EV
        tensCalc cardsInPlay =
            fromIntegral
            (
                125 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay )
            )
            /
            fromIntegral
            (413 - length cardsInPlay )


        otherCalc :: Vector Card -> Card -> EV
        otherCalc cardsInPlay card =
            fromIntegral
            (
                29 -
                length ( Data.Vector.filter (==card) cardsInPlay )
            )
            /
            fromIntegral
            (413 - length cardsInPlay)


dealerProbabilityMap4 :: Map (Vector Card) EV
dealerProbabilityMap4 = Data.Map.Lazy.fromSet checkProbability (Data.Set.fromList . Data.Vector.toList $ dealerHandList)
  where
    checkProbability :: Vector Card -> EV
    checkProbability specificDealerHand = calculateIndividualDealerHandProbability specificDealerHand
      where
        calculateIndividualDealerHandProbability
            :: Vector Card -> EV
        calculateIndividualDealerHandProbability specificDealerHand =
            go (pure $ head specificDealerHand) (tail specificDealerHand) 1
          where
            go :: Vector Card -> Vector Card -> Double -> EV
            go _ (Data.Vector.null -> True) storedProbability =
                storedProbability
            go cardsInPlay dealerHand@(Data.Vector.head -> TenJackQueenKing)
                storedProbability =
                    go (cardsInPlay `snoc` TenJackQueenKing) (tail dealerHand)
                    (tensCalc cardsInPlay * storedProbability)
            go cardsInPlay dealerHand storedProbability =
                let other = Data.Vector.head dealerHand in

                go (cardsInPlay `snoc` other) (tail dealerHand)
                (otherCalc cardsInPlay other * storedProbability)


        tensCalc :: Vector Card -> EV
        tensCalc cardsInPlay =
            fromIntegral
            (
                124 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay )
            )
            /
            fromIntegral
            (412 - length cardsInPlay )


        otherCalc :: Vector Card -> Card -> EV
        otherCalc cardsInPlay card =
            fromIntegral
            (
                28 -
                length ( Data.Vector.filter (==card) cardsInPlay )
            )
            /
            fromIntegral
            (412 - length cardsInPlay)


dealerProbabilityMap5 :: Map (Vector Card) EV
dealerProbabilityMap5 = Data.Map.Lazy.fromSet checkProbability (Data.Set.fromList . Data.Vector.toList $ dealerHandList)
  where
    checkProbability :: Vector Card -> EV
    checkProbability specificDealerHand = calculateIndividualDealerHandProbability specificDealerHand
      where
        calculateIndividualDealerHandProbability
            :: Vector Card -> EV
        calculateIndividualDealerHandProbability specificDealerHand =
            go (pure $ head specificDealerHand) (tail specificDealerHand) 1
          where
            go :: Vector Card -> Vector Card -> Double -> EV
            go _ (Data.Vector.null -> True) storedProbability =
                storedProbability
            go cardsInPlay dealerHand@(Data.Vector.head -> TenJackQueenKing)
                storedProbability =
                    go (cardsInPlay `snoc` TenJackQueenKing) (tail dealerHand)
                    (tensCalc cardsInPlay * storedProbability)
            go cardsInPlay dealerHand storedProbability =
                let other = Data.Vector.head dealerHand in

                go (cardsInPlay `snoc` other) (tail dealerHand)
                (otherCalc cardsInPlay other * storedProbability)


        tensCalc :: Vector Card -> EV
        tensCalc cardsInPlay =
            fromIntegral
            (
                123 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay )
            )
            /
            fromIntegral
            (411 - length cardsInPlay )


        otherCalc :: Vector Card -> Card -> EV
        otherCalc cardsInPlay card =
            fromIntegral
            (
                27 -
                length ( Data.Vector.filter (==card) cardsInPlay )
            )
            /
            fromIntegral
            (411 - length cardsInPlay)

dealerProbabilityMap6 :: Map (Vector Card) EV
dealerProbabilityMap6 = Data.Map.Lazy.fromSet checkProbability (Data.Set.fromList . Data.Vector.toList $ dealerHandList)
  where
    checkProbability :: Vector Card -> EV
    checkProbability specificDealerHand = calculateIndividualDealerHandProbability specificDealerHand
      where
        calculateIndividualDealerHandProbability
            :: Vector Card -> EV
        calculateIndividualDealerHandProbability specificDealerHand =
            go (pure $ head specificDealerHand) (tail specificDealerHand) 1
          where
            go :: Vector Card -> Vector Card -> Double -> EV
            go _ (Data.Vector.null -> True) storedProbability =
                storedProbability
            go cardsInPlay dealerHand@(Data.Vector.head -> TenJackQueenKing)
                storedProbability =
                    go (cardsInPlay `snoc` TenJackQueenKing) (tail dealerHand)
                    (tensCalc cardsInPlay * storedProbability)
            go cardsInPlay dealerHand storedProbability =
                let other = Data.Vector.head dealerHand in

                go (cardsInPlay `snoc` other) (tail dealerHand)
                (otherCalc cardsInPlay other * storedProbability)


        tensCalc :: Vector Card -> EV
        tensCalc cardsInPlay =
            fromIntegral
            (
                122 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay )
            )
            /
            fromIntegral
            (410 - length cardsInPlay )


        otherCalc :: Vector Card -> Card -> EV
        otherCalc cardsInPlay card =
            fromIntegral
            (
                26 -
                length ( Data.Vector.filter (==card) cardsInPlay )
            )
            /
            fromIntegral
            (410 - length cardsInPlay)

standEVMap2 :: Map CT.BoardState CT.EV
standEVMap2 = Parallelize.parallelize allNonSplitBoardStates calculateStand

standEVMap :: Map (Vector Card, Card) EV
standEVMap = parallelize calculateStandEV

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
    --in the event of a natural.

    --calculateStandEV uses a lot of cases, to avoid having to use filter.
    --In reality, filter seems cheap with vectors, so perhaps
    --this wasn't the best idea.

calculateStandEV :: (Vector Card, Card) -> EV
calculateStandEV boardPosition@(playerHand, dealerFaceUp)
        | playerHand `Data.Vector.elem`
          [[TenJackQueenKing,Ace],[Ace,TenJackQueenKing]]
           =
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
                $
                Data.Vector.filter
                (
                    ( playerHandValue == ) .
                    cardsValueOf
                )
                dealerHandSix

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
        | otherwise, playerHandValue <- cardsValueOf playerHand =
            twoWinMinus2LossMinusTie boardPosition
                dealerHandList
                Data.Vector.empty

        --providing a function to make the above boilerplate more concise.

twoWinMinus2LossMinusTie
        :: (Vector Card, Card)
            -> Vector (Vector Card)
            -> Vector (Vector Card)
            -> EV
twoWinMinus2LossMinusTie boardPosition lossPositions tiePositions =
            2 -
            (
                2 *
                calculateProbabilityFromDealerHands
                    boardPosition
                    lossPositions

            )
            -
            calculateProbabilityFromDealerHands
                boardPosition
                tiePositions


    --needs further commenting; this function should calculate
    --the value of all dealer hands by
    --implementing calculateIndividualDealerHandProbability
    --with the board position (i.e, cards in play)
    --on the individual dealer hands

calculateProbabilityFromDealerHands
        :: BoardPosition -> Vector (Vector Card) -> EV
calculateProbabilityFromDealerHands
        boardPosition@(playerHand, dealerFaceUp) dealerHands =
            sum $
            calculateIndividualDealerHandProbability boardPosition <$>
            Data.Vector.filter ((== dealerFaceUp) . head) dealerHands

    --Recursive dispatcher to internal go function. Implements true tail recursion
    --Needs to be looked over again, due to weird numbers.

calculateIndividualDealerHandProbability
        :: BoardPosition -> Vector Card -> EV
calculateIndividualDealerHandProbability (playerCards,dealerFaceUp)
        specificDealerHand =
            go (playerCards `snoc` dealerFaceUp) (tail specificDealerHand) 1
      where
        go :: Vector Card -> Vector Card -> Double -> EV
        go _ (Data.Vector.null -> True) storedProbability =
            storedProbability
        go cardsInPlay dealerHand storedProbability =
                go (cardsInPlay `snoc` head dealerHand) (tail dealerHand)
                 $ probabilityOfEventCalculator cardsInPlay (head dealerHand) * storedProbability


tensCalc :: Vector Card -> EV
tensCalc cardsInPlay =
            fromIntegral
            (
                128 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay )
            )
            /
            fromIntegral
            (416 - length cardsInPlay )


otherCalc :: Vector Card -> Card -> EV
otherCalc cardsInPlay card =
            fromIntegral
            (
                32 -
                length ( Data.Vector.filter (==card) cardsInPlay )
            )
            /
            fromIntegral
            (416 - length cardsInPlay)

-- The code succeeding this is needs considerable checking and verification.

--eta-reduction increases memory use.

evaluateHitOrStand :: BoardPosition -> Suggestion
evaluateHitOrStand boardState =
    (Data.Map.Lazy.!) hitOrStandMap boardState


hitOrStandMap :: Map BoardPosition Suggestion
hitOrStandMap = parallelize evaluateHitOrStandInner

evaluateHitOrStandInner :: (Vector Card, Card) -> Suggestion
evaluateHitOrStandInner boardState@(playerCards, dealerFaceUp) |
    6 == length playerCards =
        Suggestion (standEVMap Data.Map.Lazy.! boardState, Stand)
    | otherwise =
    Suggestion $
    max ((standEVMap Data.Map.Lazy.! boardState), Stand)
        (evaluateHit boardState, Hit)

boardPositionToBoardState :: (Vector Card, Card) -> (Vector CT.Card, CT.Card, Vector a3)
boardPositionToBoardState (hand, dealerFaceUp) =
    (unsafeCoerce hand :: Vector CT.Card, unsafeCoerce dealerFaceUp :: CT.Card, empty)


evaluateHit :: (Vector Card, Card) -> EV
evaluateHit (playerCards, dealerFaceUp) =
    sum $
    calculateHitEV dealerFaceUp .
    adjustProbabilityByNewCard dealerFaceUp <$>
    appendCardToPlayerHand playerCards


appendCardToPlayerHand :: Vector Card -> Vector (Vector Card)
appendCardToPlayerHand playerCards =
    do
        newCard <- twoToAce
        if 21 < cardsValueOf (playerCards `snoc` newCard) ||
            6 == length playerCards
                then Data.Vector.empty
                else pure $ playerCards `snoc` newCard


adjustProbabilityByNewCard :: Card -> Vector Card -> (Double , Vector Card) --THIS CODE IS PROBABLY CANCER, made while dead tired!)
adjustProbabilityByNewCard dealerFaceUp playerCards =
    (
        calculateProbabilityOfNewCard playerCards dealerFaceUp
        ,
        playerCards
    )

--modified to version that clears the additional card via init, from version using 129/33/417.

calculateProbabilityOfNewCard :: Vector Card -> Card -> Double
calculateProbabilityOfNewCard playerCards@(last -> TenJackQueenKing)
    dealerFaceUp =
        tensCalc $ init playerCards `snoc` dealerFaceUp
calculateProbabilityOfNewCard playerCards@(last -> card) dealerFaceUp =
    otherCalc (init playerCards `snoc` dealerFaceUp) card


calculateHitEV :: Card -> (Double , Vector Card) -> EV
calculateHitEV dealerFaceUp (oddsOfNewCard , playerCards ) =
    oddsOfNewCard *
    (fst . suggestion)
    (
    evaluateHitOrStandInner
        (
            Data.Vector.fromList . sort . Data.Vector.toList $ playerCards,
            dealerFaceUp
        )
    )

--The next section will be providing more complex actions, such as support for opening positions
--like splits, doubles, surrenders. After this section, we'll have the stuff that pushes
--the code into the JSON.

evaluateDoubleAction :: BoardPosition -> Suggestion --this function needs to be adjusted for how doubles affect odds
evaluateDoubleAction (playerCards, dealerFaceUp) =
    Suggestion
    (   --Did not pass testing. What?--
        (+1).(*2).subtract 1 $
        sum $
        uncurry (*) .
        fmap
        (
            (standEVMap Data.Map.Lazy.!).
            (, dealerFaceUp) .
            Data.Vector.fromList . sort . Data.Vector.toList
        )
        .
        adjustProbabilityByNewCard dealerFaceUp <$>
        appendCardToPlayerHand playerCards
        ,
        DoubleAction
    )

-- looks like split EV has to support an extended dealerHands check to consider cards removed from the shoe. But generally
-- it appears that split EV is accurate.

evaluateSplit :: BoardPosition -> Suggestion --looks like first split forbids surrender, the second split allows surrender.
evaluateSplit (playerCards, dealerFaceUp) = --as with evaluateDoubleAction, you need to find a meaningful way to adjust the EV.
    Suggestion-- on current progress, it seems to be all a matter of evaluate split. subtract two, average, plus one doesn't make sense
    --split needs to be fully recalculated, and it's likely going to create a situation wherein memoization fails.
    (
        subtract 1
        (sum $
        uncurry (*) .
        fmap
        (
            fst .
            suggestion .
            evaluateNoSplitDoubleSurrender .
            (, dealerFaceUp) .
            ( Data.Vector.fromList . sort . Data.Vector.toList )
        )
        .
        adjustProbabilityByNewCardModified dealerFaceUp (head playerCards) <$>
        appendCardToPlayerHand
        (init playerCards)
    )
        +
        sum (uncurry (*) .
        fmap
        (
            fst .
            suggestion .
            evaluateDoubleNoSurrender .
            (, dealerFaceUp) .
            ( Data.Vector.fromList . sort . Data.Vector.toList )
        )
        .
        adjustProbabilityByNewCardModified dealerFaceUp (head playerCards) <$>
        appendCardToPlayerHand
        (init playerCards))

        ,
        Split
    )
  where
    adjustProbabilityByNewCardModified :: Card -> Card -> Vector Card -> (Double, Vector Card)
    adjustProbabilityByNewCardModified dealerFaceUp playerCard playerCards =
        (
            calculateProbabilityOfNewCard (cons playerCard playerCards) dealerFaceUp
            ,
            playerCards
        )


surrender :: Suggestion
surrender = Suggestion (0.50, Surrender)

--Next are top-level functions, then the JSON producers.

evaluateDoubleNoSurrender :: BoardPosition -> Suggestion
evaluateDoubleNoSurrender boardPosition =
    Data.Vector.maximum
    (
        [
            evaluateDoubleAction boardPosition,
            evaluateHitOrStand boardPosition
        ]
    )

evaluateSplitDoubleSurrender :: BoardPosition -> Suggestion
evaluateSplitDoubleSurrender boardPosition =
    Data.Vector.maximum
    (
        [
            evaluateSplit boardPosition,
            surrender,
            evaluateDoubleAction boardPosition,
            evaluateHitOrStand boardPosition
        ]
    )

evaluateNoSplitDoubleSurrender :: BoardPosition -> Suggestion
evaluateNoSplitDoubleSurrender boardPosition =
    Data.Vector.maximum
    [
        evaluateDoubleAction boardPosition,
        surrender,
        evaluateHitOrStand boardPosition
    ]

--Going to peek ahead and compute EV, just to set up a system of tests.
--Currently, this test is failing substantially and is revealing an EV higher
--than it should be.

checkEVofGame :: Double
checkEVofGame =
    sum $
    uncurry (*) .
    bimap probabilityOfStartingHandsSplitter applyApplicableEvaluation .
    dupe <$>
    listOfStartingHands


applyApplicableEvaluation :: (Vector Card, DealerFaceUp) -> EV
applyApplicableEvaluation boardState@(playerCards, dealerFaceUp)=
    fst . suggestion $ case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b ->  Main.evaluateSplitDoubleSurrender boardState
        _ -> evaluateNoSplitDoubleSurrender boardState


listOfStartingHands :: Vector (Vector Card, Card)
listOfStartingHands =  Data.Vector.fromList . Data.Set.toList $ Data.Set.filter ((==2).length.fst) gameStateList


probabilityOfStartingHandsSplitter :: BoardPosition -> Double
probabilityOfStartingHandsSplitter boardPosition@(playerCards, dealerFaceUp) =
    case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b -> probabilityOfStartingHands boardPosition
        _ -> 2 * probabilityOfStartingHands boardPosition


probabilityOfStartingHands :: BoardPosition -> Double
probabilityOfStartingHands boardPosition@(playerCards, dealerFaceUp) =
    internalSplitter (playerCards Data.Vector.! 0) [] *
    internalSplitter (playerCards Data.Vector.! 1) [playerCards Data.Vector.! 0] *
    internalSplitter dealerFaceUp playerCards

  where

    internalSplitter TenJackQueenKing cardsInPlay =
        tensCalc cardsInPlay
    internalSplitter other cardsInPlay =
        otherCalc cardsInPlay other

--JSON producers, as well as the outputter.

writeJSONOutput :: FilePath -> IO ()
writeJSONOutput filepath = LB.writeFile filepath jsonEncodedList


jsonEncodedList :: LB.ByteString
jsonEncodedList = encode blackjackActionDirectory


blackjackActionDirectory :: BlackjackActionDirectoryTopLevel
blackjackActionDirectory =
    BlackjackActionDirectoryTopLevel $
    makeMainBranches $
    Data.Set.filter ((==2) . length . fst) gameStateList

--Eta-reduced, has implicit "setOfInputBoardPositions"

makeMainBranches :: Set BoardPosition -> Vector (GameState, BranchContents)
makeMainBranches =
    fmap appendBranches .
    Data.Vector.fromList .
    Data.Set.toList


appendBranches :: BoardPosition -> (GameState, BranchContents)
appendBranches boardPosition =
    (GameState boardPosition, BranchContents $ createBranchContents boardPosition)


createBranchContents :: BoardPosition -> Vector (GameState, AnnotatedSuggestions)
createBranchContents boardPosition =
    fmap branchContentsMaker $
    Data.Vector.fromList $
    Data.Set.toList $
    Data.Set.filter ((<6) . length . fst) $
    Data.Set.filter (startsWithUnsafeTwoOnly boardPosition) gameStateList


startsWithUnsafeTwoOnly :: BoardPosition -> BoardPosition -> Bool
startsWithUnsafeTwoOnly (prefix,_) (positionToBeChecked,_)
    | prefix Data.Vector.! 0 == positionToBeChecked Data.Vector.!0 && prefix Data.Vector.!1 == positionToBeChecked Data.Vector.!1 =
        True
    | otherwise = False


branchContentsMaker :: BoardPosition -> (GameState, AnnotatedSuggestions)
branchContentsMaker boardPosition@(playerCards,_) =
    (GameState boardPosition, AnnotatedSuggestions $ makeAnnotatedSuggestions boardPosition )


makeAnnotatedSuggestions :: BoardPosition -> Vector (AllowedActions, Suggestion, Probability)
makeAnnotatedSuggestions boardPosition
    | 2 < (length.fst) boardPosition =
        [(HitStandOnly, evaluateHitOrStand boardPosition, ())]
    | fst boardPosition Data.Vector.! 0 == fst boardPosition Data.Vector.! 1 =
        [
            (ActionsSplitSurrenderDouble, Main.evaluateSplitDoubleSurrender boardPosition, ()),
            (ActionsDouble, evaluateNoSplitDoubleSurrender boardPosition, ()),
            (HitStandOnly, evaluateHitOrStand boardPosition, ())
        ]
    | otherwise =
        [
            (ActionsSurrenderDouble, evaluateNoSplitDoubleSurrender boardPosition, ()),
            (ActionsDouble, evaluateNoSplitDoubleSurrender boardPosition, ()),
            (HitStandOnly, evaluateHitOrStand boardPosition, ())
        ]

