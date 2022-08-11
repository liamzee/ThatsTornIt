{-# LANGUAGE OverloadedStrings , OverloadedLists , ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Outputter where

import TopLevelEvaluator ( evaluateInitial )
import DataDeclarations
    ( Suggestion,
      CardsInPlay,
      Seed(Seed),
      GameTreeContents(GameTreeContents),
      BlackjackSuggestions(BlackjackSuggestions), Card (Two, Ace), DealerCards )
import GHC.Generics ( Generic )
import Data.Aeson ( encode, FromJSON, ToJSON )
--import Graphics.UI.TinyFileDialogs ( saveFileDialog, OK (OK), messageBox, IconType(Error) )
import Data.Text (empty, unpack, Text)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy ( writeFile )
import Prelude hiding (writeFile)
import CommonNamesAndFunctions ( allRanks, appendNewCardPlayer )
import HitStandEvaluator (evaluateHitVsStand)
import Data.Foldable (foldl')
import CardValueChecker (valueCheck)
import Control.Applicative ((<**>))
import Data.Functor ((<&>))
import Data.List (sort, filter)
import Data.Set
import qualified Data.Set as Set
import Data.Vector



{-filePathMaker :: IO Data.Text.Text
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
-}

--blackjackSuggestions :: BlackjackSuggestions
--blackjackSuggestions = BlackjackSuggestions $ makeGameTree <$> makeSeed
{-
allRanksVector :: Vector Card
allRanksVector = [ Two .. Ace ]

seed :: Set ( Vector Card, DealerCards)
seed = Data.Set.fromList . Data.Vector.toList $ do
    dealerCard <- allRanksVector
    leftCard <- allRanksVector
    rightCard <- allRanksVector
    pure ((sort [leftCard,rightCard]),[dealerCard])
    
makeGameTree :: Seed -> Set (CardsInPlay)
makeGameTree inputSeed = 
    
    seed <>
    appendNewElement seed <>
    appendNewElement (appendNewElement seed) <>
    appendNewElement (appendNewElement (appendNewElement seed)) <>
    appendNewElement (appendNewElement (appendNewElement (appendNewElement seed)))

appendNewElement :: Set (CardsInPlay) -> Set (CardsInPlay)
appendNewElement target = unions $ flip Set.map target $ Data.Set.fromList . (\(leftElem,rightElem) -> do

        newCard <- allRanks
        [(sort (newCard:leftElem),rightElem)])

        -}