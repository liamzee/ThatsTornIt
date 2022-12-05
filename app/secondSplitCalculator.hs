{-# LANGUAGE OverloadedLists, ViewPatterns #-}

module SecondSplitCalculator where

import Data.Vector (Vector, snoc, cons)
import CalculateTypes (Card (..), EV, PlayerCards, Probability)
import qualified Data.Vector as Vec
import qualified Data.List as List
import qualified Data.Map.Lazy as LazyMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, union)
import Data.Map.Lazy (fromSet)
import Control.DeepSeq (force)
import Control.Parallel.Strategies (rpar, rseq, runEval, using)
import CalculateDealerHands (countedDealerHands)
import CalculateProbabilityOfHand (calculateOddsOf)
import Control.Arrow ((***), Arrow ((&&&)))
import CalculateHandValue (handValueOf, checkIfBust)
import CalculateTwoToAce (twoToAce)
import Parallelize (parallelizeLazySplit)
import CalculateNonSplitBoardStates (allPlayerHands)
import Parallelize (splitEvenlySet)



getTotalEVSecondSplit :: (Card, Card, Vector Card) -> EV
getTotalEVSecondSplit (pCardTopLevel, dFUTopLevel, rCardsTopLevel) = checkEVofGame
  where
    
    partialCardsInPlay :: Vector Card
    partialCardsInPlay = rCardsTopLevel `snoc` dFUTopLevel

    checkEVofGame :: EV
    checkEVofGame =
        splitMergeAndSumWith probabilityOfStartingHand evaluateDoubleSurrender

        listOfStartingStates

    splitMergeAndSumWith :: (Vector Card -> EV ) -> (Vector Card -> EV ) -> Vector (Vector Card) -> EV -- proposed cleanup function
    splitMergeAndSumWith function1 function2 = Vec.sum . fmap (uncurry (*) . (function1 &&& function2))

    listOfStartingStates :: Vector PlayerCards
    listOfStartingStates =
        fmap (cons pCardTopLevel . pure ) twoToAce

    --the additional pCardTopLevel is added since one of the player cards is removed for the next hand.

    probabilityOfStartingHand :: PlayerCards -> Probability
    probabilityOfStartingHand (Vec.tail -> secondCard) =
        calculateOddsOf (partialCardsInPlay `snoc` pCardTopLevel) secondCard

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
            (2*)
            $ splitMergeAndSumWith
                calculateOddsOfNewCard
                (checkForBustCarrier calculateStand)
            $ appendNewCard playerCards

    calculateOddsOfNewCard :: PlayerCards -> Probability
    calculateOddsOfNewCard newPlayerHand  =
        calculateOddsOf (partialCardsInPlay <> oldCards) newCard
        -- Appending old cards to newCards is correct because
        -- the first card has to be counted twice as it's removed
        -- as a result of the split.
      where
        oldCards = Vec.init newPlayerHand
        newCard = pure $ Vec.last newPlayerHand

    checkForBustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
    checkForBustCarrier function playerCards =
          if checkIfBust playerCards
            then -1
            else function playerCards

    appendNewCard :: PlayerCards -> Vector PlayerCards
    appendNewCard playerCards = do
          snoc playerCards <$> twoToAce

    evaluateHitStand :: PlayerCards -> EV
    evaluateHitStand = ((LazyMap.!) $! parallelizeLazySplit applicablePlayerHands evaluateHitStand') . Vec.fromList . List.sort . Vec.toList
    
    applicablePlayerHands :: Set PlayerCards
    applicablePlayerHands = Set.filter (elem pCardTopLevel) allPlayerHands

    evaluateHitStand' :: PlayerCards -> EV
    evaluateHitStand' playerCards
          | 6 == Vec.length playerCards = calculateStand playerCards
          | 21 == handValueOf playerCards = calculateStand playerCards
          | otherwise =
                max
                    (   splitMergeAndSumWith
                            calculateOddsOfNewCard
                            (checkForBustCarrier evaluateHitStand)
                            $ appendNewCard playerCards
                    )
                    $ calculateStand playerCards

    calculateStand :: PlayerCards -> EV
    calculateStand =  ((LazyMap.!) $! parallelizeMappingSnd applicablePlayerHands calculateStandInner) . Vec.fromList . List.sort . Vec.toList
    
    calculateStandInner :: PlayerCards -> EV -- Algorithm is more
    calculateStandInner playerCards = -- space efficient than
                                    --directly listing
            eVWin                   --all dealercases
        *   winProbability
        +                           --Tie probability is ignored at this level
            evLoss                  --since the evTie is 0 in this setup.
        *   lossProbability

      where
        
        eVWin :: EV
        eVWin 
            | isNatural playerCards =
                1.5
            | otherwise = 1

        isNatural :: Vector Card -> Bool
        isNatural = (`Vec.elem` [[TenJackQueenKing, Ace],[Ace,TenJackQueenKing]])

        winProbability :: Probability
        winProbability =
            1 - tieProbability - lossProbability --dealerHands is calibrated to allow
                                                 --ignoring bust case, so win probability has to be derived

        evLoss :: EV
        evLoss = -1                              --notation clearer for this purpose

-- Splitter functions to generate filters of non-bust dealer hands based on what the player cards are.

        tieProbability :: Probability
        tieProbability
            | isNatural playerCards =
                probabilityUnder naturalTieFilter
            | 6 == Vec.length playerCards =
                probabilityUnder sixCardCharlieTieFilter
            | otherwise =
                probabilityUnder normalTieFilter

        lossProbability :: Probability
        lossProbability
            | isNatural playerCards =
                0
            | 6 == Vec.length playerCards =
                probabilityUnder sixCardCharlieLossFilter
            | otherwise =
                probabilityUnder normalLossFilter

-- a function to remove repeated use of filters in win / loss probability splitter.

        probabilityUnder :: (Vector Card -> Bool) -> Probability
        probabilityUnder givenFilter =
            probabilityOfEvent $ Vec.filter (givenFilter . fst)
                preFilterForDealerFaceUp

-- function that merges the count of occurences with a call on probabilities.

        probabilityOfEvent :: Vector (Vector Card, Int) -> Probability
        probabilityOfEvent listOfHands =
            Vec.sum $
            uncurry (*) .
            (calculateOddsOfDealerHand *** fromIntegral) <$>
                listOfHands

-- Once again, the initial player card double count is valid because of the removed card under split.
                
        calculateOddsOfDealerHand :: Vector Card -> Probability
        calculateOddsOfDealerHand (Vec.tail -> newDealerCards) =
            calculateOddsOf (partialCardsInPlay <> playerCards) newDealerCards

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

-- filtering seems faster than generation, mainly because my generators are so inefficient.

    preFilterForDealerFaceUp :: Vector (Vector Card, Int)
    preFilterForDealerFaceUp =
            Vec.filter ((== dFUTopLevel) . Vec.head . fst) countedDealerHands
    
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