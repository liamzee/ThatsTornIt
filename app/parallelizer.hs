module Parallelize where
    

import Control.DeepSeq (NFData, force)
import Main (BoardPosition, gameStateList)
import Data.Map.Lazy ( union, Map, fromSet )
import Control.Parallel.Strategies (runEval, rpar, rseq)
import Data.Set (size, splitAt)


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