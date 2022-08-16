{-# LANGUAGE OverloadedLists, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}

{-This file, at least for now, is going to be done as a single module.
Comments like these will split up the parts of the module, it's not good
design, but I've lost my confidence in the inliner.

The existence of shared functions also makes it somewhat harder
to understand the organization of program, which probably
led to substantial bugs with the previous iteration.-}

module Main where

import Data.Time (getCurrentTime)
import Data.Aeson ( FromJSON, ToJSON, encode )
import qualified Data.ByteString.Lazy as LB
import GHC.Generics (Generic)
import Data.Vector ( Vector, generate, (!), modify, unfoldrExactN )
import Graphics.UI.TinyFileDialogs ( saveFileDialog )
import Data.Text ( Text, unpack )
import Data.Maybe ( fromMaybe )
import Data.Vector.Generic.Mutable (write)
import Data.Vector (empty, cons, snoc, last, null, splitAt, init)
import Prelude hiding (null, length, last, splitAt, init)
import Criterion.Main
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.Vector (length)
import Control.Monad ((<=<), (>=>))


data Card
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | TenJackQueenKing
    | Ace

    deriving (Show, Generic, Eq, Ord, NFData)

instance FromJSON Card
instance ToJSON Card


data Action
    = Stand
    | Hit
    | Surrender
    | DoubleAction
    | Split

    deriving (Show, Generic, Eq, Ord, NFData )

instance FromJSON Action
instance ToJSON Action


data AllowedActions
    = ActionsWithSplit
    | ActionsWithoutSplit
    | ActionsWithoutSurrender
    | HitStandOnly

    deriving (Show, Generic, Eq, Ord, NFData )

instance FromJSON AllowedActions
instance ToJSON AllowedActions


newtype GameState
    = GameState

    {
      gameState :: ( PlayerCards , DealerFaceUp )
    }

    deriving (Show, Generic , Eq , Ord, NFData )

instance FromJSON GameState
instance ToJSON GameState


newtype BlackjackActionDirectoryTopLevel
    = BlackjackActionDirectoryTopLevel

    {
      mainBranches :: Vector ( GameState , BranchContents )
    }

    deriving (Show, Generic, NFData)

instance FromJSON BlackjackActionDirectoryTopLevel
instance ToJSON BlackjackActionDirectoryTopLevel


type DealerFaceUp = Card
type PlayerCards = Vector Card


newtype BranchContents
    = BranchContents

    {
      branchContents :: Vector ( GameState , AnnotatedSuggestions )
    }

    deriving ( Show , Generic , Eq , Ord, NFData )

instance FromJSON BranchContents
instance ToJSON BranchContents


newtype AnnotatedSuggestions
    = AnnotatedSuggestions

    {
      annotatedSuggestions :: Vector ( AllowedActions , Suggestion , Probability )
    }

    deriving ( Show , Generic , Eq , Ord , NFData )

instance FromJSON AnnotatedSuggestions
instance ToJSON AnnotatedSuggestions


type Probability = () -- not providing this functionality right now.
type EV = Double

newtype Suggestion
    = Suggestion

    {
      suggestion :: ( EV , Action )
    }

    deriving ( Show , Generic , Eq , Ord , NFData)

instance FromJSON Suggestion
instance ToJSON Suggestion


-- the Main, which only runs time benchmarking and calls
-- runOutputter

main :: IO()
main = runOutputter

{-
    print =<< getCurrentTime
    runOutputter
    print =<< getCurrentTime
-}

-- The outputter

{-
getFilePath :: IO String
getFilePath = sanitize <$> saveFileDialog "" "" [""] ""
-}

getFilePath :: IO String
getFilePath = pure "C:\\Users\\Liam\\Desktop\\crapjson.json"

sanitize :: Maybe Text -> String
sanitize =
    unpack .
    fromMaybe 
    (
        error "could not get filePath, perhaps you canceled the prompt?"
    )


runOutputter :: IO ()
runOutputter = do
    filePath <- getFilePath
    let encodedJSON = getEncodedJSON
    LB.writeFile filePath encodedJSON


getEncodedJSON :: LB.ByteString
getEncodedJSON = encode assembledPreJSON


assembledPreJSON :: BlackjackActionDirectoryTopLevel
assembledPreJSON = BlackjackActionDirectoryTopLevel makeMainBranches

-- Around this point, we begin trying to generate all player hands.


dupe :: a -> ( a , a )
dupe input = ( input , input )


makeMainBranches :: Vector ( GameState , BranchContents )
makeMainBranches =
    cons (firstGameState, deriveBranchContents firstGameState) $ unfoldrExactN 549 (dupe.processGameState) (firstGameState, deriveBranchContents firstGameState)

  where
    
    go :: Int -> ( GameState , BranchContents )
    go n = iterate processGameState (firstGameState , deriveBranchContents firstGameState) !! (n)
    
    firstGameState :: GameState
    firstGameState =
        GameState
        (
            generate 2 (const Two) ,
            Two
        )

    processGameState ::
        ( GameState , BranchContents )
        ->
        ( GameState , BranchContents )
    processGameState ( gameState , _ ) =
        (
            createNewGameState gameState ,
            deriveBranchContents ( createNewGameState gameState )
        )
    
    createNewGameState :: GameState -> GameState
    createNewGameState (GameState (contents)) =
        GameState $ addOneToContentsTwoCards contents

    addOneToContentsTwoCards :: ( Vector Card , Card ) -> ( Vector Card , Card )
    addOneToContentsTwoCards ( vector , dealerFaceUp ) =
        case ( vector!0 , vector!1 ) of
            ( Ace , Ace ) -> 
                (
                    generate 2 ( const Two ) ,
                    incrementCardAceUnsafe dealerFaceUp
                )
            ( notAce , Ace ) -> 
                (
                    modify 
                    (
                        \u -> do
                            write u 0 $ incrementCardAceUnsafe notAce
                            write u 1 $ incrementCardAceUnsafe notAce
                    )
                    vector ,
                    dealerFaceUp
                )
            ( _ , secondElement ) ->
                (
                    modify
                    (
                        \u -> write u 1 $ incrementCardAceUnsafe secondElement
                    )
                    vector ,
                    dealerFaceUp
                )


incrementCardAceUnsafe :: Card -> Card
incrementCardAceUnsafe = \case
    Two -> Three
    Three -> Four
    Four -> Five
    Five -> Six
    Six -> Seven
    Seven -> Eight
    Eight -> Nine
    Nine -> TenJackQueenKing
    TenJackQueenKing -> Ace
        
-- Now we see the first conditional player hand generation.

twoToAce :: Vector Card
twoToAce = 
    pure Two `snoc`
    Three `snoc`
    Four `snoc`
    Five `snoc`
    Six `snoc`
    Seven `snoc`
    Eight `snoc`
    Nine `snoc`
    TenJackQueenKing `snoc`
    Ace


deriveBranchContents :: GameState -> BranchContents
deriveBranchContents seedState =
    let newestCard :: GameState -> Card
        newestCard = last . fst . gameState in
    BranchContents $ parallelAppendTo4 . parallelAppendTo3 $ appendTo2 ( seedState , AnnotatedSuggestions $ makeAnnotatedSuggestions $ gameState seedState )
        where 
            parallelAppendTo3 :: Vector (GameState, AnnotatedSuggestions) -> Vector (GameState, AnnotatedSuggestions)
            parallelAppendTo3 input =
                runEval $ do
                    let (splitTop1,splitTop2) = splitAt (length input `div` 2) input
                    let (splitTop11,splitTop12) = splitAt (length splitTop1 `div` 2) splitTop1
                    let (splitTop21,splitTop22) = splitAt (length splitTop2 `div` 2) splitTop2
                    let (splitTop111,splitTop112) = splitAt (length splitTop11 `div` 2) splitTop11
                    let (splitTop121,splitTop122) = splitAt (length splitTop12 `div` 2) splitTop12
                    let (splitTop211,splitTop212) = splitAt (length splitTop21 `div` 2) splitTop21
                    let (splitTop221,splitTop222) = splitAt (length splitTop22 `div` 2) splitTop22

                    processedSplitTop111 <- rpar (force $ appendTo3 =<< splitTop111 )
                    processedSplitTop112 <- rpar (force $ appendTo3 =<< splitTop112 )
                    processedSplitTop121 <- rpar (force $ appendTo3 =<< splitTop121 )
                    processedSplitTop122 <- rpar (force $ appendTo3 =<< splitTop122 )
                    processedSplitTop211 <- rpar (force $ appendTo3 =<< splitTop211 )
                    processedSplitTop212 <- rpar (force $ appendTo3 =<< splitTop212 )
                    processedSplitTop221 <- rpar (force $ appendTo3 =<< splitTop221 )
                    processedSplitTop222 <- rpar (force $ appendTo3 =<< splitTop222 )

                    rseq processedSplitTop111 >> rseq processedSplitTop112 >> rseq processedSplitTop121 >> rseq processedSplitTop122
                    rseq processedSplitTop211 >> rseq processedSplitTop212 >> rseq processedSplitTop221 >> rseq processedSplitTop222

                    pure (processedSplitTop111 <> processedSplitTop112 <> processedSplitTop121 <> processedSplitTop122
                        <> processedSplitTop211 <> processedSplitTop212 <> processedSplitTop221 <> processedSplitTop222)

            parallelAppendTo4 :: Vector (GameState, AnnotatedSuggestions) -> Vector (GameState, AnnotatedSuggestions)
            parallelAppendTo4 input =
                runEval $ do
                    let (splitTop1,splitTop2) = splitAt (length input `div` 2) input
                    let (splitTop11,splitTop12) = splitAt (length splitTop1 `div` 2) splitTop1
                    let (splitTop21,splitTop22) = splitAt (length splitTop2 `div` 2) splitTop2
                    let (splitTop111,splitTop112) = splitAt (length splitTop11 `div` 2) splitTop11
                    let (splitTop121,splitTop122) = splitAt (length splitTop12 `div` 2) splitTop12
                    let (splitTop211,splitTop212) = splitAt (length splitTop21 `div` 2) splitTop21
                    let (splitTop221,splitTop222) = splitAt (length splitTop22 `div` 2) splitTop22

                    processedSplitTop111 <- rpar (force $ appendTo4 =<< splitTop111 )
                    processedSplitTop112 <- rpar (force $ appendTo4 =<< splitTop112 )
                    processedSplitTop121 <- rpar (force $ appendTo4 =<< splitTop121 )
                    processedSplitTop122 <- rpar (force $ appendTo4 =<< splitTop122 )
                    processedSplitTop211 <- rpar (force $ appendTo4 =<< splitTop211 )
                    processedSplitTop212 <- rpar (force $ appendTo4 =<< splitTop212 )
                    processedSplitTop221 <- rpar (force $ appendTo4 =<< splitTop221 )
                    processedSplitTop222 <- rpar (force $ appendTo4 =<< splitTop222 )

                    rseq processedSplitTop111 >> rseq processedSplitTop112 >> rseq processedSplitTop121 >> rseq processedSplitTop122
                    rseq processedSplitTop211 >> rseq processedSplitTop212 >> rseq processedSplitTop221 >> rseq processedSplitTop222

                    pure (processedSplitTop111 <> processedSplitTop112 <> processedSplitTop121 <> processedSplitTop122
                        <> processedSplitTop211 <> processedSplitTop212 <> processedSplitTop221 <> processedSplitTop222)

            appendTo2 :: (GameState, AnnotatedSuggestions) -> Vector (GameState, AnnotatedSuggestions)
            appendTo2 target = cons target $
                        do
                        newCard <- twoToAce
                        let newHand = snoc (fst $ gameState . fst $ target ) newCard
                        let dealerCard = snd $ gameState seedState
                        if newCard < (last . fst . gameState . fst $ target) || 21 < convertToValue (fst . gameState . fst $ target, newCard) 
                        then empty
                        else do
                            let newGameState = GameState (newHand , dealerCard)
                            pure ( newGameState , AnnotatedSuggestions . makeAnnotatedSuggestions . gameState $ newGameState )

            appendTo3 :: (GameState, AnnotatedSuggestions) -> Vector (GameState, AnnotatedSuggestions)
            appendTo3 test@(length . fst . gameState . fst -> 2) = pure test
            appendTo3 target = cons target $
                        do
                        newCard <- twoToAce
                        let newHand = snoc (fst $ gameState . fst $ target ) newCard
                        let dealerCard = snd $ gameState seedState
                        if newCard < (last . fst . gameState . fst $ target) || 21 < convertToValue (fst . gameState . fst $ target, newCard) 
                        then empty
                        else do
                            let newGameState = GameState (newHand , dealerCard)
                            pure ( newGameState , AnnotatedSuggestions . makeAnnotatedSuggestions . gameState $ newGameState )

            appendTo4 :: (GameState, AnnotatedSuggestions) -> Vector (GameState, AnnotatedSuggestions)
            appendTo4 test@( (3 >= ) . length . fst . gameState . fst -> True ) = pure test
            appendTo4 target = cons target $
                        do
                        newCard <- twoToAce
                        let newHand = snoc (fst $ gameState . fst $ target ) newCard
                        let dealerCard = snd $ gameState seedState
                        if newCard < (last . fst . gameState . fst $ target) || 21 < convertToValue (fst . gameState . fst $ target, newCard) 
                        then empty
                        else do
                            let newGameState = GameState (newHand , dealerCard)
                            pure ( newGameState , AnnotatedSuggestions . makeAnnotatedSuggestions . gameState $ newGameState )
                
    

convertToValue :: ( PlayerCards , Card ) -> Int
convertToValue (playerCards,card)=
    go (snoc playerCards card) 0 0
  where
    go inputVector aces value
        | null inputVector =
            case True of
                _ | 21 >= aces * 11 + value ->
                    aces * 11 + value
                _ | 0 == aces ->
                    value
                _ ->
                    go empty (aces-1) (value+1)
        | otherwise =
            case last inputVector of
                Two ->
                    go (init inputVector) aces (value+2)
                Three ->
                    go (init inputVector) aces (value+3)
                Four ->
                    go (init inputVector) aces (value+4)
                Five ->
                    go (init inputVector) aces (value+5)
                Six ->
                    go (init inputVector) aces (value+6)
                Seven ->
                    go (init inputVector) aces (value+7)
                Eight ->
                    go (init inputVector) aces (value+8)
                Nine ->
                    go (init inputVector) aces (value+9)
                TenJackQueenKing ->
                    go (init inputVector) aces (value+10)
                Ace ->
                    go (init inputVector) (aces+1) value


                    


makeAnnotatedSuggestions :: (PlayerCards, DealerFaceUp) -> Vector (AllowedActions, Suggestion, Probability)
makeAnnotatedSuggestions input@(playerCards, dealerFaceUp) =
    case length playerCards of
        2
            | playerCards!0 == playerCards!1 ->
                pure (ActionsWithSplit, Suggestion (0,Hit), ()) <>
                pure (ActionsWithoutSplit, Suggestion (0,Hit), ()) <>
                pure (ActionsWithoutSurrender, Suggestion (0,Hit), ()) <>
                pure (HitStandOnly, Suggestion (0,Hit), ())
            | otherwise ->
                pure (ActionsWithoutSplit, Suggestion (0,Hit), ()) <>
                pure (ActionsWithoutSurrender, Suggestion (0,Hit), ()) <>
                pure (HitStandOnly , Suggestion (0,Hit), ())
        _ ->
            pure (HitStandOnly, Suggestion (0,Hit), ())


--- all of the following is crap, just made to see how well my computer would perform when the code is executed.


makeSuggestion action (playerCards, dealerFaceUp) =
    case action of
        _ -> evaluateEV playerCards $! generateHands (dealerFaceUp)


evaluateEV _ _ = ()


generateHands dealerFaceUp = appendTo dealerFaceUp
  where
    appendTo target = flip snoc target $
                        do
                        newCard <- twoToAce
                        let newHand = snoc (target) newCard
                        if pure newCard > target || 21 < convertToValue (target, newCard) 
                        then empty
                        else do
                            pure newHand