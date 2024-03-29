{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Parallelize where
    

import Control.DeepSeq (NFData, force)
import Data.Map.Lazy ( union, Map, fromSet, fromList )
import Control.Parallel.Strategies (runEval, rpar, rseq, using, evalList, evalTuple2, r0, parList, Eval, Strategy)
import qualified Data.Sequence as Sequ
import CalculateTypes (BoardState, Card, EV, EVAction, SplitBoardState, PlayerCards)
import CalculateNonSplitBoardStates (allNonSplitBoardStates)
import Data.Foldable (Foldable(toList))
import Data.Sequence (Seq)
import Data.Set hiding (union)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map.Lazy as Map
import qualified Data.List
import Control.Monad ((<=<))


convertTarget :: Vector BoardState -> Set BoardState
convertTarget boardState = Set.fromList . Vec.toList $ boardState 

parallelFromSet :: Ord a => Int -> (a -> b) -> Set a -> Map a b
parallelFromSet chunkSize f s
    | Set.size s <= chunkSize = 
        Map.fromList ([(a, f a) | a <- Set.toList s] `using` evalList (evalTuple2 r0 rseq))
    | otherwise = 
        Map.unions (parallelFromSet chunkSize f <$> Set.splitRoot s `using` parList rseq)

parallelize :: Set BoardState -> (BoardState -> EV) -> Map BoardState EV
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



parallelizeLazy :: Set BoardState -> (BoardState -> a) -> Map BoardState a
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



parallelizeLazySplit :: Set PlayerCards -> (PlayerCards -> EV) -> Map PlayerCards EV
parallelizeLazySplit target conversionFunction =

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


parallelizeLazySplitEVAction :: Set PlayerCards -> (PlayerCards -> EVAction) -> Map PlayerCards EVAction
parallelizeLazySplitEVAction target conversionFunction =

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


splitEvenlySet :: (Ord k, NFData k, NFData a, Eq a) =>
    Strategy (Map k a)
splitEvenlySet element =
        pure =<< Map.unions <$>
        sequenceA (chunk element)
  where
    chunk elm | Map.empty == elm = []
    chunk elm = (rseq <=< rpar) (force (Map.take evenSize elm)) : chunk (Map.drop evenSize elm)

    evenSize = Map.size element `div` 15