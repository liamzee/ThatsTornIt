{-# LANGUAGE OverloadedStrings , OverloadedLists , ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Outputter where

import TopLevelEvaluator ( evaluateInitial, optionsWithoutSplit )
import DataDeclarations
    ( Suggestion,
      CardsInPlay,
      Seed(Seed),
      GameTreeContents(GameTreeContents),
      BlackjackSuggestions(BlackjackSuggestions), Card (Two, Ace), DealerCards, PlayerCards, JSONLabels (..) )
import Data.Aeson ( encode, FromJSON, ToJSON )
--import Graphics.UI.TinyFileDialogs ( saveFileDialog, OK (OK), messageBox, IconType(Error) )
import Data.Text (empty, unpack, Text)
import Data.Maybe (fromMaybe)
import Data.ByteString.Lazy ( writeFile )
import Prelude hiding ((++), writeFile)
import CommonNamesAndFunctions ( allRanks, appendNewCardPlayer )
import HitStandEvaluator (evaluateHitVsStand)
import CardValueChecker (valueCheck)
import Data.List (sort)
import Data.Set ( Set, fromList, toList, unions )
import qualified Data.Set as Set

--IO Code
{-
filePathMaker :: IO Data.Text.Text
filePathMaker =
   fromMaybe Data.Text.empty <$> saveFileDialog
   "That's Torn It File Target Dialog" "json" ["json"] "json"

outputMain :: IO ()
outputMain = do
    filePath <- Data.Text.unpack <$> filePathMaker
    if Prelude.null filePath
        then messageBox "That's Torn It"
             "No file provided. Exiting with error."
             Error OK >>
             error "no file provided"
        else writeFile filePath $ encode blackjackSuggestions
-}


--Code for 

blackjackSuggestions :: BlackjackSuggestions
blackjackSuggestions = BlackjackSuggestions $ seedToBlackjackContents <$> cardinalStates

seedToBlackjackContents :: Seed -> (Seed, GameTreeContents)
seedToBlackjackContents (Seed cardsInPlay) =
    (Seed cardsInPlay , GameTreeContents (transformToTuple <$> (findSameInitial (cardsInPlay))))

findSameInitial :: CardsInPlay -> [(PlayerCards, DealerCards)]
findSameInitial ([a,b],k) =
    toList $ Set.filter (\case; (c:_,e) -> c == a && e == k; _ ->  False) gameStates

transformToTuple :: CardsInPlay -> [(JSONLabels,CardsInPlay, Suggestion)]
transformToTuple input =
    case input of
        ([a,b],_) | a == b ->
            [
                splittable,
                unsplittable,
                normalPlay
            ]
        ([_,_],_) ->
            [
                unsplittable,
                normalPlay
            ]
        input -> 
            [
                normalPlay
            ]
  where
    splittable = 
        (
            FirstRoundWithSplit,
            input,
            evaluateInitial input
        )
    unsplittable =
        (
            FirstRoundWithoutSplit,
            input,
            maximum $ optionsWithoutSplit input
        )
    normalPlay =
        (
            NormalPlay,
            input,
            evaluateHitVsStand input
        )

-- Functions used to generate all game states / possible cards in play.

cardinalStates :: [Seed]
cardinalStates = toList . Set.map Seed $ Set.filter filterForDoubles gameStates

gameStates :: Set CardsInPlay
gameStates = makeGameTree gameTreeSeed

filterForDoubles :: Eq a => ([a], b) -> Bool
filterForDoubles (([x,y]),_) | x == y = True
filterForDoubles _ = False

gameTreeSeed :: Set ( [Card], [Card])
gameTreeSeed = fromList $ do
    dealerCard <- allRanks
    leftCard <- allRanks
    rightCard <- allRanks
    pure (sort [leftCard,rightCard],[dealerCard])
    
makeGameTree :: Set (CardsInPlay) -> Set (CardsInPlay)
makeGameTree inputSeed = 
    gameTreeSeed <>
    appendNewElement gameTreeSeed <>
    appendNewElement (appendNewElement gameTreeSeed) <>
    appendNewElement (appendNewElement (appendNewElement gameTreeSeed))

appendNewElement :: Set (CardsInPlay) -> Set (CardsInPlay)
appendNewElement target =
    unions $
    flip Set.map target $
    Data.Set.fromList .
    (\(leftElem,rightElem) -> do
        newCard <- allRanks
        if valueCheck 21 (>=) (sort $ newCard:leftElem)
            then [( sort $ newCard:leftElem , rightElem)]
            else [] )