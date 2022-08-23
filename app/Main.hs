{-# LANGUAGE OverloadedStrings, LambdaCase, MonadComprehensions #-}
{-# LANGUAGE OverloadedLists, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns, TupleSections #-}

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
    toList, (//), head, elem, thaw, freeze, fromList)
import Prelude hiding (head, map, sum, null, length, last,
    splitAt, init, tail, filter)
import Criterion.Main
import Control.DeepSeq ( NFData, force, deepseq )
import Control.Parallel.Strategies
import Control.Monad ((<=<), (>=>), join)
import Data.Map.Strict ( Map, fromSet, union, (!) )
import Data.Set hiding (union)
import Control.Monad.ST (runST)
import Data.List (sort)
import Debug.Trace (trace, traceShowId)
import Control.Arrow


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


type BoardPosition = (PlayerCards, DealerFaceUp)

newtype GameState
    = GameState

    {
      gameState :: BoardPosition
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
main = do
    print =<< getCurrentTime
    print $ gEye
    --writeFile "C:\\users\\liam\\desktop\\TTITest.txt" $ show standEVMap
    print =<< getCurrentTime

-- | list of all ranks in Vector form used for combination creation.

twoToAce :: Vector Card
twoToAce =
    [Two .. Ace]

-- | creating all gameStates, within a set, for chart creation.

gameStateList :: Set (Vector Card, Card)
gameStateList =
    appendToLengthNGameState 5 $
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
                Data.Set.fromList . Data.Vector.toList $
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
    Ace -> error "attempt to increment an ace"

--Key bit of code, used to calculate the value of a Vector Card.
--Please note that the separate Inner function sems to produce
--better performance, at least on ghci.

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


--Still needs to be checked for accuracy.

--The core dealerHand list, and dealerHands that are checked and summed
--by other mechanisms within the code.
            

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
    Data.Vector.filter (flip notElem dealerHandNatural) $
    Data.Vector.filter ( (==21) . cardsValueOf ) dealerHandNotSix
                                                 

dealerHandNatural :: Vector (Vector Card)
dealerHandNatural =
    [[Ace, TenJackQueenKing],[TenJackQueenKing,Ace]]


dealerHand21Lose :: Vector (Vector Card)
dealerHand21Lose =
    Data.Vector.filter
        (\u ->
        (==6) (length u) ||
        Data.Vector.elem u
            [
                [Ace, TenJackQueenKing],
                [TenJackQueenKing, Ace]
            ]
        )
        dealerHandList


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

standEVMap :: Map (Vector Card, Card) EV
standEVMap = parallelize
  where
    -- sort of a hack to enable parallelism on standEV. Seems to work well; reduces
    -- wait time by 80% on 8 threads.
    parallelize :: Map (Vector Card, Card) EV
    parallelize = runEval $ do
        let (set1, set2) = (Data.Set.splitAt (div (size standEVSet) 2) 
                standEVSet)
        let (set11, set12) = Data.Set.splitAt (div (size set1) 2) set1
        let (set21, set22) = Data.Set.splitAt (div (size set2) 2) set2
        let (set111, set112) = Data.Set.splitAt (div (size set11) 2) set11
        let (set121, set122) = Data.Set.splitAt (div (size set12) 2) set12
        let (set211, set212) = Data.Set.splitAt (div (size set21) 2) set21
        let (set221, set222) = Data.Set.splitAt (div (size set22) 2) set22

        map111 <- rpar $ force $ fromSet calculateStandEV set111
        map112 <- rpar $ force $ fromSet calculateStandEV set112
        map121 <- rpar $ force $ fromSet calculateStandEV set121
        map122 <- rpar $ force $ fromSet calculateStandEV set122
        map211 <- rpar $ force $ fromSet calculateStandEV set211
        map212 <- rpar $ force $ fromSet calculateStandEV set212
        map221 <- rpar $ force $ fromSet calculateStandEV set221
        map222 <- rpar $ force $ fromSet calculateStandEV set222
        
        rseq map111 >> rseq map112 >> rseq map121 >> rseq map122
        rseq map211 >> rseq map212 >> rseq map221 >> rseq map222
        pure $ ((union map111 map112) `union` (union map121 map122)) `union` 
            ((union map211 map212) `union` (union map221 map222))
        
    
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
    --in the event of a natural.

    --calculateStandEV uses a lot of cases, to avoid having to use filter.
    --In reality, filter seems cheap with vectors, so perhaps
    --this wasn't the best idea.

    calculateStandEV :: (Vector Card, Card) -> EV
    calculateStandEV boardPosition@(playerHand, dealerFaceUp)
        | length playerHand == 2,
          playerHand `Data.Vector.elem`
          (
            [[TenJackQueenKing,Ace],[Ace,TenJackQueenKing]]
          ) =
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

    --needs further commenting; this function should calculate
    --the value of all dealer hands by
    --implementing calculateIndividualDealerHandProbability
    --with the board position (i.e, cards in play)
    --on the individual dealer hands

    calculateProbabilityFromDealerHands
        :: (Vector Card, Card) -> Vector (Vector Card) -> EV
    calculateProbabilityFromDealerHands
        boardPosition@(playerHand, dealerFaceUp) dealerHands =
            sum $
            calculateIndividualDealerHandProbability boardPosition <$>
            ( Data.Vector.filter ((== dealerFaceUp) . head) dealerHands )

    --recursive dispatcher to internal go function. implements true tail recursion

    calculateIndividualDealerHandProbability
        :: (Vector Card, Card) -> Vector Card -> EV
    calculateIndividualDealerHandProbability (playerCards,dealerFaceUp)
        specificDealerHand =
            go (playerCards `snoc` dealerFaceUp) (tail specificDealerHand) 1
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
        
        tensCalc :: Vector Card -> EV ---does the +1 need to be done to card odds here? Reverted, but needs checking.
        tensCalc cardsInPlay = --test verification seems to suggest the +1 isn't necessary, but it's perfectly
            fromIntegral --possible that the tests are wrong. Then again, I'm using two stacks this time.
            ( --might need to add to the denominator, who knows.
                128 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay)
            )
            /
            fromIntegral
            (416 - length cardsInPlay )
        
        otherCalc :: Vector Card -> Card -> EV
        otherCalc cardsInPlay card =
            fromIntegral
            (   
                32 -
                length ( Data.Vector.filter (==card) cardsInPlay)
            )
            /
            fromIntegral
            (416 - length cardsInPlay)

-- The code succeding this is needs considerable checking and verification.

evaluateHitOrStand :: (Vector Card, Card) -> Suggestion
evaluateHitOrStand boardState =
    parallelize Data.Map.Strict.! boardState
  where
    
    parallelize :: Map (Vector Card, Card) Suggestion
    parallelize = runEval $ do
        let (set1, set2) = (Data.Set.splitAt (div (size gameStateList) 2) 
                gameStateList)
        let (set11, set12) = Data.Set.splitAt (div (size set1) 2) set1
        let (set21, set22) = Data.Set.splitAt (div (size set2) 2) set2
        let (set111, set112) = Data.Set.splitAt (div (size set11) 2) set11
        let (set121, set122) = Data.Set.splitAt (div (size set12) 2) set12
        let (set211, set212) = Data.Set.splitAt (div (size set21) 2) set21
        let (set221, set222) = Data.Set.splitAt (div (size set22) 2) set22

        map111 <- rpar $ force $ fromSet evaluateHitOrStandInner set111
        map112 <- rpar $ force $ fromSet evaluateHitOrStandInner set112
        map121 <- rpar $ force $ fromSet evaluateHitOrStandInner set121
        map122 <- rpar $ force $ fromSet evaluateHitOrStandInner set122
        map211 <- rpar $ force $ fromSet evaluateHitOrStandInner set211
        map212 <- rpar $ force $ fromSet evaluateHitOrStandInner set212
        map221 <- rpar $ force $ fromSet evaluateHitOrStandInner set221
        map222 <- rpar $ force $ fromSet evaluateHitOrStandInner set222
        
        rseq map111 >> rseq map112 >> rseq map121 >> rseq map122
        rseq map211 >> rseq map212 >> rseq map221 >> rseq map222
        pure $ ((union map111 map112) `union` (union map121 map122)) `union` 
            ((union map211 map212) `union` (union map221 map222))

evaluateHitOrStandInner :: (Vector Card, Card) -> Suggestion
evaluateHitOrStandInner boardState@(playerCards, dealerFaceUp) =
    Suggestion $
    max (standEVMap Data.Map.Strict.! boardState, Stand)
        (evaluateHit boardState, Hit)


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
        if 21 < (cardsValueOf $ playerCards `snoc` newCard) ||
            length playerCards == 6
                then Data.Vector.empty
                else pure $ playerCards `snoc` newCard


adjustProbabilityByNewCard :: Card -> Vector Card -> (Double , Vector Card) --THIS CODE IS PROBABLY CANCER, made while dead tired!)
adjustProbabilityByNewCard dealerFaceUp playerCards =
    (
        calculateProbabilityOfNewCard playerCards dealerFaceUp
        ,
        playerCards
    )

--needs checking, the 129 instead of 128 is supposed to compensate for the fact that
--we're rewinding the latest playercard to see the odds of it being drawn, so we
--pretend it's not already in the cardsInPlay.
--Same applies to 33 and 417.

calculateProbabilityOfNewCard :: Vector Card -> Card -> Double
calculateProbabilityOfNewCard playerCards@(last -> TenJackQueenKing)
    dealerFaceUp =
        let cardsInPlay = playerCards `snoc` dealerFaceUp in
        fromIntegral
        (
            129 -
            length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay)
        )
        /
        fromIntegral
        ( 417 - length cardsInPlay )
calculateProbabilityOfNewCard playerCards@(last ->card) dealerFaceUp =
    let cardsInPlay = playerCards `snoc` dealerFaceUp in
            
    fromIntegral
    (   
        33 -
        length ( Data.Vector.filter (==card) cardsInPlay)
    )
    /
    fromIntegral
    (417 - length cardsInPlay)


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


multiplyFirstBySecondInTuple :: (Double, Double) -> Double
multiplyFirstBySecondInTuple (a,b) = a*b


evaluateDoubleAction :: BoardPosition -> Suggestion --this function needs to be adjusted for how doubles affect odds
evaluateDoubleAction (playerCards, dealerFaceUp) =
    Suggestion
    (   --Did not pass testing. What?--
        (+1).(*2).subtract 1 $ --provisional estimate of how doubles affect odds
        sum $
        multiplyFirstBySecondInTuple .
        fmap (standEVMap Data.Map.Strict.!) .
        fmap ((, dealerFaceUp). ( Data.Vector.fromList . sort . Data.Vector.toList )) .
        adjustProbabilityByNewCard dealerFaceUp <$>
        appendCardToPlayerHand playerCards
        ,
        DoubleAction
    )

evaluateSplit :: BoardPosition -> Suggestion
evaluateSplit (playerCards, dealerFaceUp) = --as with evaluateDoubleAction, you need to find a meaningful way to adjust the EV.
    Suggestion
    (
        (+1).(*2).subtract 1 $  --provisional estimate of how doubles affect odds
        sum $
        multiplyFirstBySecondInTuple .
        fmap 
        (
            fst .
            suggestion .
            evaluateDoubleNoSurrender .
            (, dealerFaceUp) .
            ( Data.Vector.fromList . sort . Data.Vector.toList )
        )
        .
        adjustProbabilityByNewCard dealerFaceUp <$>
        appendCardToPlayerHand
        (
            init playerCards
        )
        ,
        Split
    )

surrender :: Suggestion
surrender = Suggestion (0.50, Surrender)

--Next are top-level functions, then the JSON producers.

evaluateDoubleNoSurrender :: BoardPosition -> Suggestion
evaluateDoubleNoSurrender boardPosition =
    maximum
    (
        [
            evaluateDoubleAction boardPosition,
            evaluateHitOrStand boardPosition
        ]
        :: Vector Suggestion
    )

evaluateSplitDoubleSurrender :: BoardPosition -> Suggestion
evaluateSplitDoubleSurrender boardPosition =
    
    maximum
    (
        [
            evaluateSplit boardPosition,
            surrender,
            evaluateDoubleAction boardPosition,
            evaluateHitOrStand boardPosition
        ]
        :: Vector Suggestion
    )

evaluateNoSplitDoubleSurrender :: BoardPosition -> Suggestion
evaluateNoSplitDoubleSurrender boardPosition =
    
    maximum
    (
        [
            evaluateDoubleAction boardPosition,
            surrender,
            evaluateHitOrStand boardPosition
        ]
        :: Vector Suggestion
    )

--Going to peek ahead and compute EV.



gEye :: Double
gEye =
    sum $
    multiplyFirstBySecondInTuple <$>
    second applyApplicableEvaluation <$>
    first probabilityOfStartingHandsSplitter <$>
    dupe <$>
    listOfStartingHands

applyApplicableEvaluation :: (Vector Card, DealerFaceUp) -> EV
applyApplicableEvaluation boardState@(playerCards, dealerFaceUp)=
    fst . suggestion $ case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b ->  evaluateSplitDoubleSurrender boardState
        _ -> evaluateNoSplitDoubleSurrender boardState

listOfStartingHands :: Vector (Vector Card, Card)
listOfStartingHands =  Data.Vector.fromList . Data.Set.toList $ Data.Set.filter ((==2).length.fst) gameStateList

probabilityOfStartingHandsSplitter :: BoardPosition -> Double
probabilityOfStartingHandsSplitter boardPosition@(playerCards, dealerFaceUp) =
    case (playerCards Data.Vector.! 0 , playerCards Data.Vector.! 1) of
        (a , b) | a == b -> 1*probabilityOfStartingHands boardPosition
        _ -> 2 * probabilityOfStartingHands boardPosition

probabilityOfStartingHands :: BoardPosition -> Double
probabilityOfStartingHands boardPosition@(playerCards, dealerFaceUp) =
    internalSplitter (playerCards Data.Vector.! 0) [] * internalSplitter (playerCards Data.Vector.! 1) [playerCards Data.Vector.! 0] *
    internalSplitter (dealerFaceUp) playerCards

  where

    internalSplitter TenJackQueenKing cardsInPlay =
        tensCalc cardsInPlay
    internalSplitter other cardsInPlay =
        otherCalc cardsInPlay other

    tensCalc :: Vector Card -> EV ---does the +1 need to be done to card odds here? Reverted, but needs checking.
    tensCalc cardsInPlay = --test verification seems to suggest the +1 isn't necessary, but it's perfectly
            fromIntegral --possible that the tests are wrong. Then again, I'm using two stacks this time.
            ( --might need to add to the denominator, who knows.
                128 -
                length ( Data.Vector.filter (==TenJackQueenKing) cardsInPlay)
            )
            /
            fromIntegral
            (416 - length cardsInPlay )
        
    otherCalc :: Vector Card -> Card -> EV
    otherCalc cardsInPlay card =
            fromIntegral
            (   
                32 -
                length ( Data.Vector.filter (==card) cardsInPlay)
            )
            /
            fromIntegral
            (416 - length cardsInPlay)