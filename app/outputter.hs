{-# LANGUAGE OverloadedLists #-}

module Outputter where
import qualified Data.ByteString.Lazy as LB
import Main hiding (makeAnnotatedSuggestions, startsWithUnsafeTwoOnly, branchContentsMaker, createBranchContents, appendBranches, makeMainBranches, blackjackActionDirectory, jsonEncodedList)
import qualified Data.Set
import Data.Set (Set)
import Data.Vector (Vector, fromList, (!))
import Data.Aeson (encode)

--JSON producers, as well as the outputter.

writeJSONOutput :: FilePath -> IO ()
writeJSONOutput filepath = LB.writeFile filepath jsonEncodedList


jsonEncodedList :: LB.ByteString
jsonEncodedList = encode blackjackActionDirectory


blackjackActionDirectory :: BlackjackActionDirectoryTopLevel
blackjackActionDirectory =
    BlackjackActionDirectoryTopLevel $
    makeMainBranches $
    Data.Set.filter ((==2) . length . fst) gameStateList

--Eta-reduced, has implicit "setOfInputBoardPositions"

makeMainBranches :: Set BoardPosition -> Vector (GameState, BranchContents)
makeMainBranches =
    fmap appendBranches .
    Data.Vector.fromList .
    Data.Set.toList


appendBranches :: BoardPosition -> (GameState, BranchContents)
appendBranches boardPosition =
    (GameState boardPosition, BranchContents $ createBranchContents boardPosition)


createBranchContents :: BoardPosition -> Vector (GameState, AnnotatedSuggestions)
createBranchContents boardPosition =
    fmap branchContentsMaker $
    Data.Vector.fromList $
    Data.Set.toList $
    Data.Set.filter ((<6) . length . fst) $
    Data.Set.filter (startsWithUnsafeTwoOnly boardPosition) gameStateList


startsWithUnsafeTwoOnly :: BoardPosition -> BoardPosition -> Bool
startsWithUnsafeTwoOnly (prefix,_) (positionToBeChecked,_)
    | prefix Data.Vector.! 0 == positionToBeChecked Data.Vector.!0 && prefix Data.Vector.!1 == positionToBeChecked Data.Vector.!1 =
        True
    | otherwise = False


branchContentsMaker :: BoardPosition -> (GameState, AnnotatedSuggestions)
branchContentsMaker boardPosition@(playerCards,_) =
    (GameState boardPosition, AnnotatedSuggestions $ makeAnnotatedSuggestions boardPosition )


makeAnnotatedSuggestions :: BoardPosition -> Vector (AllowedActions, Suggestion, Probability)
makeAnnotatedSuggestions boardPosition
    | 2 < (length.fst) boardPosition =
        [(HitStandOnly, evaluateHitOrStand boardPosition, ())]
    | fst boardPosition Data.Vector.! 0 == fst boardPosition Data.Vector.! 1 =
        [
            (ActionsSplitSurrenderDouble, evaluateSplitDoubleSurrender boardPosition, ()),
            (ActionsDouble, evaluateNoSplitDoubleSurrender boardPosition, ()),
            (HitStandOnly, evaluateHitOrStand boardPosition, ())
        ]
    | otherwise =
        [
            (ActionsSurrenderDouble, evaluateNoSplitDoubleSurrender boardPosition, ()),
            (ActionsDouble, evaluateNoSplitDoubleSurrender boardPosition, ()),
            (HitStandOnly, evaluateHitOrStand boardPosition, ())
        ]
