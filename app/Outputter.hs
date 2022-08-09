{-# LANGUAGE OverloadedStrings , ApplicativeDo #-}

module Outputter where

import TopLevelEvaluator ( evaluateInitial )
import DataDeclarations
    ( Suggestion,
      CardsInPlay,
      Seed(Seed),
      GameTreeContents(GameTreeContents),
      BlackjackSuggestions(BlackjackSuggestions), Card )
import GHC.Generics ( Generic )
import Data.Aeson ( encode, FromJSON, ToJSON )
import Graphics.UI.TinyFileDialogs ( saveFileDialog, OK (OK), messageBox, IconType(Error) )
import Data.Text (empty, unpack, Text)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy ( writeFile )
import Prelude hiding (writeFile)
import CommonNamesAndFunctions ( allRanks, appendNewCardPlayer )
import HitStandEvaluator (evaluateHitVsStand)
import Data.Foldable (foldl')
import Data.List (sort)
import CardValueChecker (valueCheck)
import Control.Applicative ((<**>))



filePathMaker :: IO Data.Text.Text
filePathMaker =
    
   fromMaybe Data.Text.empty <$> saveFileDialog
   "That's Torn It File Target Dialog" "" [".json"] "JSONs"



outputMain :: IO ()
outputMain = do
    
    filePath <- Data.Text.unpack <$> filePathMaker
    if null filePath
        then messageBox "That's Torn It"
             "No file provided. Exiting with error."
             Error OK 
             >> error "no file provided"
        else writeFile filePath $ encode blackjackSuggestions



blackjackSuggestions :: BlackjackSuggestions
blackjackSuggestions = BlackjackSuggestions $ makeGameTree <$> makeSeed

removeSeqDuplicate :: Eq a => [a] -> [a]
removeSeqDuplicate [] = []
removeSeqDuplicate [b] = [b]
removeSeqDuplicate (a:b:xs) | a == b = removeSeqDuplicate (a:xs)
    | otherwise = a: removeSeqDuplicate (b:xs)

injectASort :: Seed -> Seed
injectASort (Seed (left, right)) = Seed (sort left, right)

makeSeed :: [Seed]
makeSeed = removeSeqDuplicate . sort $ injectASort . Seed <$> do
    leftSideOne <- allRanks
    leftSideTwo <- allRanks
    rightSideOne <- allRanks
    pure (([leftSideOne,leftSideTwo]), [rightSideOne])



makeGameTree :: Seed -> (Seed, GameTreeContents)
makeGameTree (Seed seed) = (Seed seed , GameTreeContents (go1 seed) )
    
  where



    go1 :: CardsInPlay -> [Suggestion]
    go1 cardsInPlay = (evaluateInitial cardsInPlay : go2 cardsInPlay)



    go2 :: CardsInPlay -> [Suggestion]
    go2 cardsInPlay

        | null seedForNew = []
        | otherwise = foldl' (flip (:)) recursionField ( evaluateHitVsStand <$> seedForNew )

      where
    
        seedForNew = appendNewCardPlayer cardsInPlay
        recursionField = concat $ go2 <$> seedForNew







makeSeed2 :: [Seed]
makeSeed2 = Seed <$> do
    leftSideOne <- allRanks
    leftSideTwo <- allRanks
    rightSideOne <- allRanks
    pure (([leftSideOne,leftSideTwo]), [rightSideOne])

test :: [[Card]]
test = removeSeqDuplicate . sort . fmap sort $ appendNewCard . appendNewCard . appendNewCard . appendNewCard $ ( map fst . removeSeqDuplicate . sort . fmap internalsort $ makeSeed2)

internalsort :: Seed -> ([Card],[Card])
internalsort (Seed (leftList, rightList)) = (sort leftList, rightList)


appendNewCard :: [[Card]] -> [[Card]]
appendNewCard preExistingCards =

    let 
        outputCards =
          
            filter ( valueCheck 21 (>=) ) $
            ( preExistingCards ) <**>
            (flip (++) <$> pure <$> allRanks) in

          --Last value filters preExistingCards based on player hitting,
          --starting at the end. The ((:) <$> deck) <*> adds cards, then
          --the filter before it removes combinations that would cause the
          --dealer to go bust.

    preExistingCards <> outputCards