{-# LANGUAGE OverloadedLists, TupleSections, BangPatterns #-}

module EvaluateSplit where
import CalculateTypes (BoardState, EVAction, Action (..), EV, Card (TenJackQueenKing, Ace), PlayerCards, DealerFaceUp, RemovedCards, SplitBoardState)
import Data.Vector (snoc, Vector, cons)
import qualified Data.Vector as Vec
import CalculateTwoToAce (twoToAce, twoToAceSet)
import Control.Applicative (liftA2, liftA3)
import Control.Arrow ((&&&), Arrow ((***)))
import CalculateProbabilityOfHand (calculateOddsOf, boardStateToCardsInPlay)
import Data.Set (Set, cartesianProduct)
import qualified Data.Set as Set
import CalculateDealerHands (uniqueDealerHandsIncludingBust, countedDealerHands)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Merge (sort)
import Control.Parallel.Strategies (runEval, rpar, rseq, NFData)
import Data.Map.Strict (fromSet)
import Control.DeepSeq (force)
import Data.Map (Map, union, (!))
import CalculateNonSplitBoardStates (allPlayerHandsIncludingBust, allPlayerHands)
import CalculateHandValue (checkIfBust, handValueOf)
import qualified Data.List
import Parallelize (parallelizeLazySplit)
import qualified Data.Map.Lazy as LazyMap

evaluateSplitResult :: Card -> Card -> EVAction
evaluateSplitResult playerCard dealerCard =
    Vec.maximum
        [ splitDouble
        , splitHitOrStand (generatedPlayerHand, dealerCard)
        ]

  where
    generatedPlayerHand :: Vector Card
    generatedPlayerHand = playerCard `Vec.cons` pure playerCard -- this is wrong, has to be evaluated through all possible split hands


    splitDouble :: EVAction
    splitDouble = 
      ( (2*). Vec.sum --RESULT HAS TO BE DOUBLED, REMEMBER THAT!
      $ fmap
          (uncurry (*)
        . (splitFirstStandEV &&& splitFirstCalculateOddsOfNewCard)
        . Vec.snoc generatedPlayerHand)
          twoToAce , Split)
    
    splitFirstCalculateOddsOfNewCard :: Vector Card -> Double
    splitFirstCalculateOddsOfNewCard newHand = calculateOddsOf (Vec.init newHand <> pure playerCard <> pure dealerCard) (pure $ Vec.last newHand)

    splitFirstStandEV :: Vector Card -> EV
    splitFirstStandEV = undefined

    splitHitOrStand :: BoardState -> EVAction
    splitHitOrStand = undefined

allPossibleRemovedCards :: Set (Vector Card)
allPossibleRemovedCards = parallelize allPlayerHandsIncludingBust appendAllDealerHandsSorted
  where
    appendAllDealerHandsSorted :: Vector Card -> Set (Vector Card)
    appendAllDealerHandsSorted inputHand
      | 2 == Vec.length inputHand = 
          Set.map vectorCardSort $ Set.map (<> inputHand) twoCardDealerHands
          `Set.union` Set.map (<> inputHand) uniqueDealerHandsIncludingBust
      | checkIfBust inputHand = Set.map vectorCardSort $ Set.map (<> inputHand) twoCardDealerHands
      | otherwise = Set.map vectorCardSort $ Set.map (<> inputHand) uniqueDealerHandsIncludingBust


    twoCardDealerHands :: Set (Vector Card)
    twoCardDealerHands = Set.unions $ Set.map createDealerHands twoToAceSet
    
    createDealerHands :: Card -> Set (Vector Card)
    createDealerHands firstElement =
        Set.map ((firstElement `cons`).pure ) twoToAceSet


    
    vectorCardSort :: Vector Card -> Vector Card
    vectorCardSort hand = runST $ do
        mvec <- Vec.thaw hand
        sort mvec
        Vec.freeze mvec


parallelize :: (Ord a, NFData a, Ord b, NFData b) => Set a -> (a -> Set b) -> Set b
parallelize target conversionFunction =

    runEval $ do
        let (set1, set2) =
                Set.splitAt (div (length target) 2) target
        let (set11, set12) = Set.splitAt (div (length set1) 2) set1
        let (set21, set22) = Set.splitAt (div (length set2) 2) set2
        let (set111, set112) = Set.splitAt (div (length set11) 2) set11
        let (set121, set122) = Set.splitAt (div (length set12) 2) set12
        let (set211, set212) = Set.splitAt (div (length set21) 2) set21
        let (set221, set222) = Set.splitAt (div (length set22) 2) set22
        let (set1111, set1112) = Set.splitAt (div (length set22) 2) set111
        let (set1121, set1122) = Set.splitAt (div (length set22) 2) set112
        let (set1211, set1212) = Set.splitAt (div (length set22) 2) set121
        let (set1221, set1222) = Set.splitAt (div (length set22) 2) set122
        let (set2111, set2112) = Set.splitAt (div (length set22) 2) set211
        let (set2121, set2122) = Set.splitAt (div (length set22) 2) set212
        let (set2211, set2212) = Set.splitAt (div (length set22) 2) set221
        let (set2221, set2222) = Set.splitAt (div (length set22) 2) set222

        map1111 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1111
        map1112 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1112
        map1121 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1121
        map1122 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1122

        map1211 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1211
        map1212 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1212
        map1221 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1221
        map1222 <- rpar $ force $ Set.unions $ Set.map conversionFunction set1222

        map2111 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2111
        map2112 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2112
        map2121 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2121
        map2122 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2122

        map2211 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2211
        map2212 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2212
        map2221 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2221
        map2222 <- rpar $ force $ Set.unions $ Set.map conversionFunction set2222
        

        rseq map1111 >> rseq map1112 >> rseq map1121 >> rseq map1122
        rseq map1211 >> rseq map1212 >> rseq map1221 >> rseq map1222
        rseq map2111 >> rseq map2112 >> rseq map2121 >> rseq map2122
        rseq map2211 >> rseq map2212 >> rseq map2221 >> rseq map2222
        pure $ (((map1111 `Set.union` map1112) `Set.union` (map1121 `Set.union` map1122)) `Set.union`
            (map1211 `Set.union` map1212) `Set.union` (map1221 `Set.union` map1222)) `Set.union`
            (((map2111 `Set.union` map2112) `Set.union` (map2121 `Set.union` map2122)) `Set.union`
            ((map2211 `Set.union` map2212) `Set.union` (map2221 `Set.union` map2222)))


setForSplitEVStands :: Set (Card, Card, Vector Card)
setForSplitEVStands = parallelize allPossibleSplits createAllPossibleStartingStatesSecond
  where
    allPossibleSplits :: Set (Card, Card)
    allPossibleSplits = cartesianProduct twoToAceSet twoToAceSet

    createAllPossibleStartingStatesSecond :: (Card, Card) -> Set (Card, Card, Vector Card)
    createAllPossibleStartingStatesSecond (playerCard, dealerCard) =
        if playerCard /= dealerCard
          then Set.map (playerCard, dealerCard,) $ Set.filter (\u -> elem dealerCard u && elem playerCard u) allPossibleRemovedCards
          else Set.map (playerCard, dealerCard,) $ Set.filter (\u -> 2 <= Vec.length (Vec.filter (==playerCard) u)) allPossibleRemovedCards


{-- prototype code, might be necessary to post a cleaned up version to ensure full coverage for the first split stand EV --}
setForSplitEVStands' :: Set (Card, Card, Vector Card)
setForSplitEVStands' = parallelize allPossibleSplits createAllPossibleStartingStatesSecond
  where
    allPossibleSplits :: Set (Card, Card)
    allPossibleSplits = cartesianProduct twoToAceSet twoToAceSet

    createAllPossibleStartingStatesSecond :: (Card, Card) -> Set (Card, Card, Vector Card)
    createAllPossibleStartingStatesSecond (playerCard, dealerCard) =
        Set.map (playerCard, dealerCard,) $ flip parallelize (Set.singleton . vectorCardSort) $
            parallelize (Set.filter (Vec.elem playerCard) allPlayerHandsIncludingBust) appendAllDealerHands
      where
        appendAllDealerHands :: Vector Card -> Set (Vector Card)
        appendAllDealerHands inputHand
            | 2 == Vec.length inputHand = 
                Set.map vectorCardSort $ Set.map (<> inputHand) twoCardDealerHands
                `Set.union` Set.map (<> inputHand) (Set.filter ((==dealerCard). Vec.head)uniqueDealerHandsIncludingBust)
            | checkIfBust inputHand = Set.map vectorCardSort $ Set.map (<> inputHand) twoCardDealerHands
            | otherwise = Set.map vectorCardSort $ Set.map (<> inputHand) (Set.filter ((==dealerCard). Vec.head)uniqueDealerHandsIncludingBust)

              where

                twoCardDealerHands :: Set (Vector Card)
                twoCardDealerHands = Set.unions $ Set.map createDealerHands twoToAceSet
    
                createDealerHands :: Card -> Set (Vector Card)
                createDealerHands firstElement =
                    Set.map ((firstElement `cons`).pure ) twoToAceSet


    
        vectorCardSort :: Vector Card -> Vector Card
        vectorCardSort hand = runST $ do
                    mvec <- Vec.thaw hand
                    sort mvec
                    Vec.freeze mvec

secondGameEVMap :: Map (Card, Card, Vector Card) EV
secondGameEVMap = parallelizeMapping setForSplitEVStands getTotalEVSecondSplit

benchMarkEV :: Map (Card, Card, Vector Card) EV
benchMarkEV = parallelizeMapping (Set.take 2000 setForSplitEVStands) getTotalEVSecondSplit

parallelizeMapping :: (Ord a, NFData a, Ord b, NFData b) => Set a -> (a -> b) -> Map a b
parallelizeMapping target conversionFunction =

    runEval $ do
        let (set1, set2) =
                Set.splitAt (div (length target) 2) target
        let (set11, set12) = Set.splitAt (div (length set1) 2) set1
        let (set21, set22) = Set.splitAt (div (length set2) 2) set2
        let (set111, set112) = Set.splitAt (div (length set11) 2) set11
        let (set121, set122) = Set.splitAt (div (length set12) 2) set12
        let (set211, set212) = Set.splitAt (div (length set21) 2) set21
        let (set221, set222) = Set.splitAt (div (length set22) 2) set22
        let (set1111, set1112) = Set.splitAt (div (length set22) 2) set111
        let (set1121, set1122) = Set.splitAt (div (length set22) 2) set112
        let (set1211, set1212) = Set.splitAt (div (length set22) 2) set121
        let (set1221, set1222) = Set.splitAt (div (length set22) 2) set122
        let (set2111, set2112) = Set.splitAt (div (length set22) 2) set211
        let (set2121, set2122) = Set.splitAt (div (length set22) 2) set212
        let (set2211, set2212) = Set.splitAt (div (length set22) 2) set221
        let (set2221, set2222) = Set.splitAt (div (length set22) 2) set222

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

getTotalEVSecondSplit :: (Card, Card, Vector Card) -> EV
getTotalEVSecondSplit (pCardTopLevel, dFUTopLevel, rCardsTopLevel) = checkEVofGame
  where
    
    checkEVofGame :: Double
    checkEVofGame =
        Vec.sum $
        uncurry (*) .
        (probabilityOfStartingHandsSplitter &&& evaluateDoubleSurrender)
        <$>
        listOfStartingStates

    listOfStartingStates :: Vector PlayerCards
    listOfStartingStates =
        fmap (cons pCardTopLevel . pure ) twoToAce


    probabilityOfStartingHandsSplitter :: PlayerCards -> Double
    probabilityOfStartingHandsSplitter playerCards =
        calculateOddsOf (rCardsTopLevel `snoc` pCardTopLevel `snoc` dFUTopLevel) (pure $ Vec.last playerCards)

    evaluateDoubleSurrender :: PlayerCards -> EV
    evaluateDoubleSurrender playerCards =
      Vec.maximum
        [
            calculateDouble,
            surrender,
            evaluateHitStand playerCards
        ]

      where

        surrender :: EV
        surrender = -0.50

        calculateDouble :: EV
        calculateDouble =
            (2*). Vec.sum $ --remember to double the result!
            uncurry (*) .
            (calculateOddsOfNewCard &&& checkForBustCarrier calculateStand)
            <$>
            appendNewCard playerCards

    checkForBustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
    checkForBustCarrier function playerCards =
          if checkIfBust playerCards
            then -1
            else function $ Vec.fromList . Data.List.sort $ Vec.toList playerCards

    appendNewCard :: PlayerCards -> Vector PlayerCards
    appendNewCard playerCards = do
          snoc playerCards <$> twoToAce

    calculateOddsOfNewCard :: PlayerCards -> EV
    calculateOddsOfNewCard newPlayerHand  =
        calculateOddsOf (rCardsTopLevel <> Vec.init newPlayerHand `snoc` dFUTopLevel) $ pure $ Vec.last newPlayerHand

    evaluateHitStand :: PlayerCards -> EV
    evaluateHitStand = (parallelizeLazySplit makeApplicableBoardStates evaluateHitStand' LazyMap.!)
      where
    evaluateHitStand' :: PlayerCards -> EV
    evaluateHitStand' playerCards
          | 6 == Vec.length playerCards = calculateStand playerCards
          | otherwise =
                max
                    (Vec.sum
                        $ uncurry (*)
                        . (calculateOddsOfNewCard &&& checkForBustCarrier evaluateHitStand)
                        <$>
                        appendNewCard playerCards
                    )
                    $ calculateStand playerCards

    calculateStand :: PlayerCards -> EV
    calculateStand = (parallelizeMappingSnd makeApplicableBoardStates calculateStandInner Data.Map.!)

    makeApplicableBoardStates :: Set PlayerCards
    makeApplicableBoardStates = Set.filter (elem pCardTopLevel) allPlayerHands
    
    calculateStandInner :: PlayerCards -> EV
    calculateStandInner playerCards =

            eVWin
        *   winProbability
        +
            evLoss
        *   lossProbability

      where

        winProbability :: Double
        winProbability =
            1 - tieProbability - lossProbability
    
        evLoss :: Double
        evLoss = -1

        isNatural :: Vector Card -> Bool
        isNatural = (`Vec.elem` [[TenJackQueenKing, Ace],[Ace,TenJackQueenKing]])

        eVWin :: Double
        eVWin 
            | isNatural playerCards =
                1.5
            | otherwise = 1

-- A splitter function to evaluate player cases.

        tieProbability :: Double
        tieProbability
            | isNatural playerCards =
                probabilityUnder naturalTieFilter
            | 6 == Vec.length playerCards =
                probabilityUnder sixCardCharlieTieFilter
            | otherwise =
                probabilityUnder normalTieFilter

-- Basically a splitter function that feeds the boardState to a probabilityOfEvent calculator.

        lossProbability :: Double
        lossProbability
            | isNatural playerCards =
                0
            | 6 == Vec.length playerCards =
                probabilityUnder sixCardCharlieLossFilter
            | otherwise =
                probabilityUnder normalLossFilter

-- a function to remove repeated use of filters in win / loss probability splitter.

        probabilityUnder :: (Vector Card -> Bool) -> EV
        probabilityUnder givenFilter =
            probabilityOfEvent playerCards $ Vec.filter (givenFilter . fst)
                preFilterForDealerFaceUp

-- the subsequent lines are filters for remaining dealerhands that 
-- correspond to a player hand situation, producing both
-- winning and losing hands for a given situation.

-- | tie if there's a dealer natural.

        naturalTieFilter :: Vector Card -> Bool
        naturalTieFilter =
            isNatural

-- | tie if the opponent has a six card charlie of equal value.

        sixCardCharlieTieFilter :: Vector Card -> Bool
        sixCardCharlieTieFilter item =
            6 == Vec.length item &&
                handValueOf playerCards == handValueOf item

-- | lose if the dealer has a natural, or if the dealer has a six
-- card charlie that doesn't bust and is of higher value than the
-- player's

        sixCardCharlieLossFilter :: Vector Card -> Bool
        sixCardCharlieLossFilter item =
            isNatural item ||
                (6 == Vec.length item &&
            handValueOf playerCards < handValueOf item)

-- | tie if the opponent has a non-six card charlie hand,
-- non-natural hand of equal value.

        normalTieFilter :: Vector Card -> Bool
        normalTieFilter item =
            not (isNatural item) &&
                6 /= Vec.length item &&
                handValueOf playerCards == handValueOf item

-- | lose if the dealer has a natural, a non-busting six card charlie,
-- or a non-busting hand of higher value

        normalLossFilter :: Vector Card -> Bool
        normalLossFilter item =
            6 == Vec.length item ||
                isNatural item ||
                handValueOf playerCards < handValueOf item

    probabilityOfEvent :: PlayerCards -> Vector (Vector Card, Int) -> Double
    probabilityOfEvent playerCards listOfHands =
            Vec.sum $
            uncurry (*) .
            (calculateOddsOfDealerHand playerCards *** fromIntegral) <$>
                listOfHands

-- Calls a general-purpose odds calculator for the odds of a player hand.
-- This one preprocesses because preFilterForDealerFaceUp assigns the first card
-- a probability of one, and all other first cards a probability of zero.

    calculateOddsOfDealerHand :: PlayerCards -> Vector Card -> Double
    calculateOddsOfDealerHand playerCards assessedHand =
            calculateOddsOf (boardStateToCardsInPlay (playerCards <> rCardsTopLevel, dFUTopLevel))
            (Vec.tail assessedHand)

-- Filters away dealerHands that don't have the correct initial card.

    preFilterForDealerFaceUp :: Vector (Vector Card, Int)
    preFilterForDealerFaceUp =
            Vec.filter ((== dFUTopLevel) . Vec.head . fst) countedDealerHands


{-
    parallelizeLazy :: Set SplitBoardState -> (SplitBoardState -> EV) -> Map SplitBoardState EV
    parallelizeLazy target conversionFunction =

      runEval $ do
        let (set1, set2) =
                Set.splitAt (div (length target) 2) target
        let (set11, set12) = Set.splitAt (div (length set1) 2) set1
        let (set21, set22) = Set.splitAt (div (length set2) 2) set2
        let (set111, set112) = Set.splitAt (div (length set11) 2) set11
        let (set121, set122) = Set.splitAt (div (length set12) 2) set12
        let (set211, set212) = Set.splitAt (div (length set21) 2) set21
        let (set221, set222) = Set.splitAt (div (length set22) 2) set22

        map1111 <- rpar $ fromSet conversionFunction set111
        map1112 <- rpar $ fromSet conversionFunction set112
        map1121 <- rpar $ fromSet conversionFunction set121
        map1122 <- rpar $ fromSet conversionFunction set122

        map1211 <- rpar $ fromSet conversionFunction set211
        map1212 <- rpar $ fromSet conversionFunction set212
        map1221 <- rpar $ fromSet conversionFunction set221
        map1222 <- rpar $ fromSet conversionFunction set222
        

        rseq map1111 >> rseq map1112 >> rseq map1121 >> rseq map1122
        rseq map1211 >> rseq map1212 >> rseq map1221 >> rseq map1222
        pure $ ((map1111 `union` map1112) `union` (map1121 `union` map1122)) `union`
            (map1211 `union` map1212) `union` (map1221 `union` map1222)
-}
    
parallelizeMappingSnd :: Set PlayerCards -> (PlayerCards -> EV) -> Map PlayerCards EV
parallelizeMappingSnd target conversionFunction =

      runEval $ do
        let (set1, set2) =
                Set.splitAt (div (length target) 2) target
        let (set11, set12) = Set.splitAt (div (length set1) 2) set1
        let (set21, set22) = Set.splitAt (div (length set2) 2) set2
        let (set111, set112) = Set.splitAt (div (length set11) 2) set11
        let (set121, set122) = Set.splitAt (div (length set12) 2) set12
        let (set211, set212) = Set.splitAt (div (length set21) 2) set21
        let (set221, set222) = Set.splitAt (div (length set22) 2) set22
        let (set1111, set1112) = Set.splitAt (div (length set22) 2) set111
        let (set1121, set1122) = Set.splitAt (div (length set22) 2) set112
        let (set1211, set1212) = Set.splitAt (div (length set22) 2) set121
        let (set1221, set1222) = Set.splitAt (div (length set22) 2) set122
        let (set2111, set2112) = Set.splitAt (div (length set22) 2) set211
        let (set2121, set2122) = Set.splitAt (div (length set22) 2) set212
        let (set2211, set2212) = Set.splitAt (div (length set22) 2) set221
        let (set2221, set2222) = Set.splitAt (div (length set22) 2) set222

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