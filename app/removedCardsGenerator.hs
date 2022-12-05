{-# LANGUAGE TupleSections #-}

module RemovedCardsGenerator where

import Data.Vector (Vector, cons, snoc)
import CalculateTypes (Card, EV)
import Data.Set (Set, cartesianProduct)
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Control.DeepSeq (NFData)
import Data.Map (Map)
import SecondSplitCalculator (getTotalEVSecondSplit)
import Control.Monad.ST (runST)
import Data.Vector.Algorithms.Merge (sort)
import CalculateTwoToAce (twoToAceSet, twoToAce)
import CalculateHandValue (checkIfBust, checkForSoft17)
import Control.Parallel.Strategies (rseq, rpar, runEval, using)
import Control.DeepSeq (force)
import Data.Map.Lazy (fromSet, union)
import qualified Data.Vector.Mutable as Vm
import Control.Applicative (liftA2)
import qualified Data.List
import Parallelize (splitEvenlySet)



secondGameEVMap :: Map (Card, Card, Vector Card) EV
secondGameEVMap = parallelizeMapping setForSplitEVStands getTotalEVSecondSplit

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
        Set.map (playerCard, dealerCard,) $ Set.map vectorCardSort $
            parallelize (Set.filter (Vec.elem playerCard) allPlayerHandsIncludingBust) appendAllDealerHands
      where
        appendAllDealerHands :: Vector Card -> Set (Vector Card)
        appendAllDealerHands inputHand
            | checkIfBust inputHand = Set.map vectorCardSort $ Set.map (<> inputHand) twoCardDealerHands
            | otherwise = Set.map vectorCardSort $ Set.map (<> inputHand) (Set.filter ((== dealerCard). Vec.head) uniqueDealerHandsIncludingBust)

              where

                twoCardDealerHands :: Set (Vector Card)
                twoCardDealerHands = Set.map ( vectorCardSort . cons dealerCard . pure ) twoToAceSet


    
        vectorCardSort :: Vector Card -> Vector Card
        vectorCardSort hand = runST $ do
                    mvec <- Vec.thaw hand
                    sort mvec
                    Vec.freeze mvec



allPlayerHandsIncludingBust :: Set (Vector Card)
allPlayerHandsIncludingBust = Set.map vectorSort $ Set.unions $ Set.map appendRemainder playerHandsBase

  where

    playerHandsBase :: Set (Vector Card)
    playerHandsBase =
        Set.map vectorSort $ Set.unions $ Set.map createPlayerBases twoToAceSet

    createPlayerBases :: Card -> Set (Vector Card)
    createPlayerBases firstElement =
        Set.map createPlayerBasesInner twoToAceSet
      where
        createPlayerBasesInner :: Card -> Vector Card
        createPlayerBasesInner secondElement = firstElement `cons` pure secondElement

    appendRemainder :: Vector Card -> Set (Vector Card)
    appendRemainder playerCards =
        if 6 == Vec.length playerCards || checkIfBust playerCards
            then Set.singleton playerCards
            else Set.singleton playerCards `Set.union` newHands
      where
        newHands :: Set (Vector Card)
        newHands = Set.unions $ Set.map appendRemainder $ Set.map (playerCards `snoc`) twoToAceSet

    
    vectorSort :: Vector Card -> Vector Card
    vectorSort hand = runST $ do
                    mvec <- Vec.thaw hand
                    sort mvec
                    Vec.freeze mvec


uniqueDealerHandsIncludingBust :: Set (Vector Card)
uniqueDealerHandsIncludingBust =
    Set.fromList . Vec.toList $ internalSort <$> dealerHands -- refactor here, should perform faster
  where

    dealerHands :: Vector (Vector Card)
    dealerHands =
        dealerHandsCore >>= appendToCore

    dealerHandsCore :: Vector (Vector Card)
    dealerHandsCore =
        liftA2 (snoc . pure) twoToAce twoToAce

    appendToCore :: Vector Card -> Vector (Vector Card)
    appendToCore hand

        | 6 == Vec.length hand || checkForSoft17 hand = -- checkIfBust is implied by checkForSoft17
            pure hand
        | otherwise = twoToAce >>= appendToCore . snoc hand -- wrote away from do notation version.

    vectorSort :: Vector (Vector Card) -> Vector (Vector Card) -- vector sort version makes the computation faster as the set isn't filtering the duplicates.
    vectorSort hand = runST $ do
        mvec <- Vec.thaw hand
        sort mvec
        Vec.freeze mvec

    internalSort :: Vector Card -> Vector Card
    internalSort hand = runST $ do
        mvec <- Vec.thaw hand
        sort $ Vm.slice 1 (Vec.length hand - 1) mvec
        Vec.freeze mvec

{-}
uniqueDealerHandsIncludingBust' :: Set (Vector Card)
uniqueDealerHandsIncludingBust' = 
    Set.fromList . fmap Data.List.head . Vec.group $ Set.map internalSort dealerHands

  where

    dealerHands :: Set (Vector Card)

    
    internalSort :: Vector Card -> Vector Card
    internalSort hand = runST $ do
        mvec <- Vec.thaw hand
        sort $ Vm.slice 1 (Vec.length hand - 1) mvec
        Vec.freeze mvec
--}


parallelizeMapping :: Set (Card, Card, Vector Card) -> ((Card, Card, Vector Card) -> EV) -> Map (Card, Card, Vector Card) EV
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