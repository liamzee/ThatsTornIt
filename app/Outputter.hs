{-# LANGUAGE OverloadedStrings #-}

module Outputter where

import TopLevelEvaluator
import DataDeclarations
import GHC.Generics ( Generic )
import Data.Aeson ( encode, FromJSON, ToJSON )
import Graphics.UI.TinyFileDialogs ( saveFileDialog, OK (OK), messageBox )
import Data.Text (empty, unpack, Text)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy ( writeFile )
import Prelude hiding (writeFile)
import CommonNamesAndFunctions
import Graphics.UI.TinyFileDialogs (IconType(Error))
import HitStandEvaluator (evaluateHitVsStand)



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
        Error OK >> error "no file provided"
        else writeFile filePath $ encode blackjackSuggestions



blackjackSuggestions :: BlackjackSuggestions
blackjackSuggestions = undefined BlackjackSuggestions $ makeGameTree <$> makeSeed



makeSeed :: [Seed]
makeSeed = Seed <$> do
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
        | otherwise = foldr (:) recursionField ( evaluateHitVsStand <$> seedForNew )

      where
    
        seedForNew = appendNewCardPlayer cardsInPlay
        recursionField = concat $ go2 <$> seedForNew