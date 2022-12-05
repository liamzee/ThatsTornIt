{-# LANGUAGE OverloadedLists, TupleSections #-}

module EvaluateSplit where
import CalculateTypes (BoardState, EVAction, Action (..), EV, Card (TenJackQueenKing, Ace), PlayerCards, DealerFaceUp, RemovedCards, SplitBoardState, Probability)
import Data.Vector (snoc, Vector, cons)
import qualified Data.Vector as Vec
import CalculateTwoToAce (twoToAce, twoToAceSet)
import Control.Applicative (liftA2, liftA3)
import Control.Arrow ((&&&), Arrow ((***)))
import CalculateProbabilityOfHand (calculateOddsOf, boardStateToCardsInPlay)
import Data.Set (Set, cartesianProduct)
import qualified Data.Set as Set
import CalculateDealerHands (countedDealerHands, countedDealerHandsIncludingBust)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Merge (sort)
import Control.Parallel.Strategies (runEval, rpar, rseq, NFData, using)
import Data.Map.Strict (fromSet)
import Control.DeepSeq (force)
import Data.Map (Map, union, (!))
import CalculateNonSplitBoardStates (allPlayerHandsIncludingBust, allPlayerHands)
import CalculateHandValue (checkIfBust, handValueOf)
import qualified Data.List
import Parallelize (parallelizeLazySplit, splitEvenlySet)
import qualified Data.Map.Lazy as LazyMap
import CalculateStand (isNatural)
import Debug.Trace (trace, traceShowId)
import Data.Maybe (fromMaybe)
import SecondSplitCalculator (getTotalEVSecondSplit, parallelizeMappingSnd)
import RemovedCardsGenerator (setForSplitEVStands)
import Parallelize (parallelizeLazySplitEVAction)

--- okay, can't be arsed, but I assume the error in the code is hanging around here somewhere.


firstSplit :: Card -> Card -> Map (Card, Card, Vector Card) EV -> EVAction
firstSplit pCardTopLevel dCardTopLevel mapping =
    (Vec.sum
    $ uncurry (*)
    . (calculateOddsOf (pure pCardTopLevel `snoc` dCardTopLevel `snoc` pCardTopLevel) . Vec.drop 1 &&& fst . evaluateDoubleHitStandSplitter)
    <$>
    allPlayerHandsStartFirstSplit, Split)
  where
--    standardRemovedCards :: Vector Card
--    standardRemovedCards = pure pCardTopLevel `snoc` dCardTopLevel -- part of removed cards clause, removed as standard. Initial player card is removed twice as part of split conditions.

    allPlayerHandsStartFirstSplit :: Vector (Vector Card)
    allPlayerHandsStartFirstSplit = snoc (pure pCardTopLevel) <$> twoToAce
    
    evaluateDoubleHitStandSplitter
        | otherwise = (,undefined). evaluateDoubleHitStand'
        -- | otherwise = evaluateDoubleHitStand dCardTopLevel pCardTopLevel mapping
    evaluateDoubleHitStand' :: PlayerCards -> EV
    evaluateDoubleHitStand' playerCards =
        max (evaluateDoubleFirstSplit playerCards) (evaluateHitOrStand playerCards)

    evaluateDoubleFirstSplit :: PlayerCards -> EV -- looks like you got confused by the generate possibilities crap, then calculate odds of such
    evaluateDoubleFirstSplit playerCards =
        Vec.sum
        $ uncurry (*)
        . (calculateOddsOf ((pure pCardTopLevel `snoc` dCardTopLevel) <> Vec.init playerCards) . Vec.drop 2 &&& bustCarrier calculateStandDouble)
        <$>
        (snoc playerCards <$> twoToAce) -- add another card to a vector of hands, modeling dealer stand.
      where
        
        bustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
        bustCarrier function playerHand =
            if checkIfBust playerHand
                then bustEVCallDouble playerHand
                else function playerHand

        
        bustEVCallDouble :: PlayerCards -> EV
        bustEVCallDouble playerHand =
            subtract 2 . Vec.sum --subtract 2 to model player losing double the money
            $ uncurry (*)
            . (calculateOddsOf (playerHand <> (pure pCardTopLevel `snoc` dCardTopLevel)) . Vec.drop 1
            &&& (mapping LazyMap.!) . (pCardTopLevel , dCardTopLevel, )
            . Vec.fromList . Data.List.sort . Vec.toList . (<> playerHand))
            <$> possibleDealerHands

          where
            possibleDealerHands :: Vector (Vector Card)
            possibleDealerHands = cons dCardTopLevel . pure <$> twoToAce
    
    calculateStandDouble :: PlayerCards -> EV
    calculateStandDouble = ((parallelizeMapping allPlayerHandsStandDouble calculateStandDouble') LazyMap.!) . Vec.fromList . Data.List.sort . Vec.toList

    
    allPlayerHandsStandDouble :: Set (Vector Card)
    allPlayerHandsStandDouble = Set.unions $ Set.map appendAllStands twoToAceSet
      where
        appendAllStands :: Card -> Set (Vector Card)
        appendAllStands card = Set.map (Vec.fromList . Data.List.sort . Vec.toList . (`snoc` card)) $ Set.fromList $ Vec.toList allPlayerHandsStartFirstSplit

 -- up to here right now, ugh
    calculateStandDouble' :: Vector Card -> EV
    calculateStandDouble' playerHand =
            Vec.sum
        $ uncurry (*)
        . ( uncurry (*)
        . (calculateOddsOf (playerHand <> (pure pCardTopLevel `snoc` dCardTopLevel)) . Vec.drop 1 . fst 
            &&& fromIntegral . snd 
          )
        &&& evaluateHandValue . fst)
        <$> applicableDealerHands

      where

        applicableDealerHands :: Vector (Vector Card, Int)
        applicableDealerHands
            | checkIfBust playerHand = (,1) . cons dCardTopLevel . pure <$> twoToAce
            | otherwise = Vec.filter ((== dCardTopLevel ) . Vec.head . fst ) countedDealerHandsIncludingBust

        evaluateHandValue :: Vector Card -> EV
        evaluateHandValue item =
            winLossDraw + examiner
          where
            examiner = mapping LazyMap.! ( pCardTopLevel, dCardTopLevel , Vec.fromList . Data.List.sort . Vec.toList $ (playerHand <> item))
            winLossDraw
                | isNatural item = -2
                | checkIfBust item = 2
                | 6 == length item = -2
                | handValueOf playerHand > handValueOf item = 2
                | handValueOf playerHand == handValueOf item = 0
                | handValueOf playerHand < handValueOf item = -2

    evaluateHitOrStand :: PlayerCards -> EV
    evaluateHitOrStand = (parallelizeLazySplit allPlayerHands evaluateHitOrStand' LazyMap.!) .  Vec.fromList . Data.List.sort . Vec.toList

    evaluateHitOrStand' :: PlayerCards -> EV
    evaluateHitOrStand' playerCards
        | 6 == length playerCards = calculateStand playerCards
        | 21 == handValueOf playerCards = calculateStand playerCards
        | otherwise = max (calculateStand playerCards)
            $ Vec.sum
            $ uncurry (*)
            . ((calculateOddsOf (playerCards <> (pure pCardTopLevel `snoc` dCardTopLevel)) . pure . Vec.last )
                &&& bustCarrier evaluateHitOrStand)
            <$> (snoc playerCards <$> twoToAce)


      where
        bustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
        bustCarrier function playerHand =
            if checkIfBust playerHand
                then bustEVCall playerHand
                else function playerHand

        
    
        bustEVCall :: PlayerCards -> EV
        bustEVCall playerHand =
            subtract 1 . Vec.sum
            $ uncurry (*)
            . (calculateOddsOf (playerHand <> (pure pCardTopLevel `snoc` dCardTopLevel)) . Vec.drop 1
                &&& (LazyMap.!) mapping . (pCardTopLevel, dCardTopLevel,)
                    . Vec.fromList . Data.List.sort . Vec.toList . (<> playerHand))
            <$> possibleDealerHands

          where
            possibleDealerHands :: Vector (Vector Card)
            possibleDealerHands = cons dCardTopLevel . pure <$> twoToAce



    calculateStand :: PlayerCards -> EV
    calculateStand = ((parallelizeMapping (Set.filter (elem pCardTopLevel) allPlayerHands) calculateStand') LazyMap.!) . Vec.fromList . Data.List.sort . Vec.toList

    calculateStand' :: PlayerCards -> EV
    calculateStand' playerHand =
        Vec.sum
        $ uncurry (*)
        . ( uncurry (*)
            . (calculateOddsOf (playerHand <> (pure pCardTopLevel `snoc` dCardTopLevel) ) . Vec.drop 1 . fst
            &&& fromIntegral . snd )
                &&& evaluateHandValue . fst)
        <$> Vec.filter ((== dCardTopLevel ) . Vec.head . fst ) countedDealerHandsIncludingBust


      where
        evaluateHandValue :: Vector Card -> EV
        evaluateHandValue item =
            winLossDraw
            + mapping LazyMap.!
            ( pCardTopLevel
            , dCardTopLevel
            , Vec.fromList . Data.List.sort . Vec.toList
                $ playerHand <> item)
          where
            winLossDraw
                | isNatural playerHand && not (isNatural item) = 1.5
                | isNatural playerHand && isNatural item = 0
                | isNatural item = -1
                | checkIfBust item = 1
                | 6 == length playerHand && 6 /= length item = 1
                | 6 == length playerHand && handValueOf playerHand > handValueOf item = 1
                | 6 == length playerHand && handValueOf playerHand == handValueOf item = 0
                | 6 == length item = -1
                | handValueOf playerHand > handValueOf item = 1
                | handValueOf playerHand == handValueOf item = 0
                | handValueOf playerHand < handValueOf item = -1



--}
benchMarkEV :: Map (Card, Card, Vector Card) EV
benchMarkEV = (LazyMap.fromSet $!) (getTotalEVSecondSplit $!) ((Set.take 2000 $! setForSplitEVStands))

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



evaluateDoubleHitStand :: Card -> Card -> Map (Card, Card, Vector Card) EV -> PlayerCards -> EVAction
evaluateDoubleHitStand dealerFaceUp playerSplitCard mapping playerCards =
        max (evaluateDoubleFirstSplit playerCards, Split) (evaluateHitOrStand dealerFaceUp playerSplitCard mapping playerCards)

  where
    evaluateDoubleFirstSplit :: PlayerCards -> EV -- looks like you got confused by the generate possibilities crap, then calculate odds of such
    evaluateDoubleFirstSplit playerCards =
        Vec.sum
        $ uncurry (*)
        . (calculateOddsOf ((pure playerSplitCard `snoc` dealerFaceUp) `snoc` playerSplitCard) . Vec.drop 2 &&& bustCarrier calculateStandDouble)
        <$>
        (snoc playerCards <$> twoToAce) -- add another card to a vector of hands, modeling dealer stand.
      where
        
        bustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
        bustCarrier function playerHand =
            if checkIfBust playerHand
                then bustEVCallDouble playerHand
                else function playerHand

        
        bustEVCallDouble :: PlayerCards -> EV
        bustEVCallDouble playerHand =
            subtract 2 . Vec.sum --subtract 2 to model player losing double the money
            $ uncurry (*)
            . (calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerFaceUp)) . Vec.drop 1
            &&& (\u -> fromMaybe (error $ "bustEVCallDouble" <> show u) (mapping LazyMap.!? u)) . (playerSplitCard , dealerFaceUp, )
            . Vec.fromList . Data.List.sort . Vec.toList . (<> playerHand))
            <$> possibleDealerHands

          where
            possibleDealerHands :: Vector (Vector Card)
            possibleDealerHands = cons dealerFaceUp . pure <$> twoToAce
    
    calculateStandDouble :: PlayerCards -> EV
    calculateStandDouble = (\u -> fromMaybe (error $ "calculateStanddouble" <> show u) ((parallelizeMapping allPlayerHandsStandDouble calculateStandDouble' ) LazyMap.!? u)) . Vec.fromList . Data.List.sort . Vec.toList

    
    allPlayerHandsStandDouble :: Set (Vector Card)
    allPlayerHandsStandDouble = Set.unions $ Set.map appendAllStands twoToAceSet
      where
        appendAllStands :: Card -> Set (Vector Card)
        appendAllStands card = Set.map (Vec.fromList . Data.List.sort . Vec.toList . (`snoc` card)) $ Set.fromList $ Vec.toList allPlayerHandsStartFirstSplit

    
    allPlayerHandsStartFirstSplit :: Vector (Vector Card)
    allPlayerHandsStartFirstSplit = snoc (pure playerSplitCard) <$> twoToAce
    

 -- up to here right now, ugh
    calculateStandDouble' :: Vector Card -> EV
    calculateStandDouble' playerHand =
            Vec.sum
        $ uncurry (*)
        . ( uncurry (*)
        . (calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerFaceUp)) . Vec.drop 1 . fst 
            &&& fromIntegral . snd 
          )
        &&& evaluateHandValue . fst)
        <$> applicableDealerHands

      where

        applicableDealerHands :: Vector (Vector Card, Int)
        applicableDealerHands
            | checkIfBust playerHand = (,1) . cons dealerFaceUp . pure <$> twoToAce
            | otherwise = Vec.filter ((== dealerFaceUp ) . Vec.head . fst ) countedDealerHandsIncludingBust

        evaluateHandValue :: Vector Card -> EV
        evaluateHandValue item =
            winLossDraw + examiner
          where
            examiner = (\u -> fromMaybe (error $ "evaluateHandValueCalculateStandDouble'" <> show u) $ mapping LazyMap.!? u) ( playerSplitCard, dealerFaceUp , Vec.fromList . Data.List.sort . Vec.toList $ (playerHand <> item))
            winLossDraw
                | isNatural item = -2
                | checkIfBust item = 2
                | 6 == length item = -2
                | handValueOf playerHand > handValueOf item = 2
                | handValueOf playerHand == handValueOf item = 0
                | handValueOf playerHand < handValueOf item = -2


evaluateHitOrStand :: Card -> Card -> Map (Card, Card, Vector Card) EV -> PlayerCards -> EVAction
evaluateHitOrStand dealerFaceUp playerSplitCard mapping = (\u -> fromMaybe (error $ "evaluateHitOrStand" <> show u) (parallelizeLazySplitEVAction allPlayerHands evaluateHitOrStand' LazyMap.!? u)) .  Vec.fromList . Data.List.sort . Vec.toList
  where
    evaluateHitOrStand' :: PlayerCards -> EVAction
    evaluateHitOrStand' playerCards
        | 6 == length playerCards = (calculateStand dealerFaceUp playerSplitCard mapping playerCards, Stand)
        | 21 == handValueOf playerCards = (calculateStand dealerFaceUp playerSplitCard mapping playerCards, Stand)
        | otherwise = max (calculateStand dealerFaceUp playerSplitCard mapping playerCards, Stand)
            $ (Vec.sum
            $ uncurry (*)
            . ((calculateOddsOf (playerCards <> (pure playerSplitCard `snoc` dealerFaceUp)) . pure . Vec.last )
                &&& bustCarrier (fst . evaluateHitOrStand dealerFaceUp playerSplitCard mapping))
            <$> (snoc playerCards <$> twoToAce), Hit)


      where
        bustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
        bustCarrier function playerHand =
            if checkIfBust playerHand
                then bustEVCall playerHand
                else function playerHand

        
    
        bustEVCall :: PlayerCards -> EV
        bustEVCall playerHand =
            subtract 1 . Vec.sum
            $ uncurry (*)
            . (calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerFaceUp)) . Vec.drop 1
                &&& (\u -> fromMaybe (error $ "bustEVCall Hit Stand" <> show u) ((LazyMap.!?) mapping u )) . (playerSplitCard, dealerFaceUp,)
                    . Vec.fromList . Data.List.sort . Vec.toList . (<> playerHand))
            <$> possibleDealerHands

          where
            possibleDealerHands :: Vector (Vector Card)
            possibleDealerHands = cons dealerFaceUp . pure <$> twoToAce



calculateStand :: Card -> Card -> Map (Card, Card, Vector Card) EV -> PlayerCards -> EV
calculateStand dealerFaceUp playerSplitCard mapping = (\u -> fromMaybe (error $ "calculateStand" <> show u) ((parallelizeMapping (Set.filter (elem playerSplitCard) allPlayerHands) calculateStand') LazyMap.!? u)) . Vec.fromList . Data.List.sort . Vec.toList
  where
    calculateStand' :: PlayerCards -> EV
    calculateStand' playerHand =
        Vec.sum
        $ uncurry (*)
        . ( uncurry (*)
            . (calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerFaceUp) ) . Vec.drop 1 . fst
            &&& fromIntegral . snd )
                &&& evaluateHandValue . fst)
        <$> Vec.filter ((== dealerFaceUp) . Vec.head . fst ) countedDealerHandsIncludingBust


      where
        evaluateHandValue :: Vector Card -> EV
        evaluateHandValue item =
            winLossDraw
            + (\u -> fromMaybe (error $ "evaluateHandValue stand" <> show u) (mapping LazyMap.!? u))
            ( playerSplitCard
            , dealerFaceUp
            , Vec.fromList . Data.List.sort . Vec.toList
                $ playerHand <> item)
          where
            winLossDraw
                | isNatural playerHand && not (isNatural item) = 1.5
                | isNatural playerHand && isNatural item = 0
                | isNatural item = -1
                | checkIfBust item = 1
                | 6 == length playerHand && 6 /= length item = 1
                | 6 == length playerHand && handValueOf playerHand > handValueOf item = 1
                | 6 == length playerHand && handValueOf playerHand == handValueOf item = 0
                | 6 == length item = -1
                | handValueOf playerHand > handValueOf item = 1
                | handValueOf playerHand == handValueOf item = 0
                | handValueOf playerHand < handValueOf item = -1
