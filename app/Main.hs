{-# LANGUAGE OverloadedLists, OverloadedStrings, LambdaCase #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ViewPatterns #-}

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
import Graphics.UI.TinyFileDialogs ( saveFileDialog )
import Data.Text ( Text, unpack )
import Data.Maybe ( fromMaybe )
import Data.Vector.Generic.Mutable (write)
import Data.Vector ( Vector, generate, (!), modify,
    empty, cons, snoc, last,
    null, splitAt, init, length,
    sum, tail, filter, unfoldrExactN)
import Prelude hiding (sum, null, length, last,
    splitAt, init, tail, filter)
import Criterion.Main
import Control.DeepSeq
import Control.Parallel.Strategies
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

    deriving (Show, Generic, Eq, Ord, Enum, NFData)

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
    = ActionsSplitSurrenderDouble
    | ActionsSurrenderDouble
    | ActionsDouble
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
type DealerHand = Vector Card


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
    go n = iterate processGameState (firstGameState , deriveBranchContents firstGameState) !! n

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
    createNewGameState (GameState contents) =
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

--At this point, we have the player hands ready, and we are now going to build the infrastructure that presents the player actions and outcomes.

makeAnnotatedSuggestions :: (PlayerCards, DealerFaceUp) -> Vector (AllowedActions, Suggestion, Probability)
makeAnnotatedSuggestions input@(playerCards, dealerFaceUp) =
    case length playerCards of
        2
            | playerCards!0 == playerCards!1 ->
                pure
                (
                    ActionsSplitSurrenderDouble
                    ,
                    Suggestion $
                    makeSuggestion ActionsSplitSurrenderDouble $
                    GameState input
                    ,
                    ()
                )
                <>
                pure
                (
                    ActionsSurrenderDouble
                    ,
                    Suggestion $
                    makeSuggestion ActionsSurrenderDouble $
                    GameState input
                    ,
                    ()
                )
                <>
                pure
                (
                    ActionsDouble
                    ,
                    Suggestion $
                    makeSuggestion ActionsDouble $
                    GameState input
                    ,
                    ()
                )
                <>
                pure
                (
                    HitStandOnly
                    ,
                    Suggestion $
                    makeSuggestion HitStandOnly $
                    GameState input
                    ,
                    ()
                )
            | otherwise ->
                pure
                (
                    ActionsSurrenderDouble
                    ,
                    Suggestion $
                    makeSuggestion ActionsSurrenderDouble $
                    GameState input
                    ,
                    ()
                )
                <>
                pure
                (
                    ActionsDouble
                    ,
                    Suggestion $
                    makeSuggestion ActionsDouble $
                    GameState input
                    ,
                    ()
                )
                <>
                pure
                (
                    HitStandOnly
                    ,
                    Suggestion $
                    makeSuggestion HitStandOnly $
                    GameState input,
                    ()
                )
        _ ->
            pure
            (
                HitStandOnly
                ,
                Suggestion $
                makeSuggestion HitStandOnly $
                GameState input
                ,
                ()
            )

-- Now we actually calculate the suggestions.

-- | filter elements to consider allowedActions

makeSuggestion :: AllowedActions -> GameState -> (EV, Action)
makeSuggestion allowedActions (GameState gameState) =
    case allowedActions of
        ActionsSplitSurrenderDouble ->
            maximum
            (
                pure (doubleAction gameState) `snoc`
                splitAction gameState `snoc`
                surrender `snoc`
                hitOrStand gameState
            )
        ActionsSurrenderDouble ->
            maximum
            (
                pure (doubleAction gameState) `snoc`
                surrender `snoc`
                hitOrStand gameState
            )
        ActionsDouble ->
            maximum
            (
                pure (doubleAction gameState) `snoc`
                hitOrStand gameState
            )
        HitStandOnly ->
            hitOrStand gameState

        where
            surrender = ( 0.5 , Surrender )


doubleAction :: (PlayerCards, DealerFaceUp) -> (EV, Action)
doubleAction input = (0,Stand) --error i just want to get this to compile


splitAction :: (PlayerCards, DealerFaceUp) -> (EV, Action)
splitAction input = (0,Stand) --error I just want to get this to compile)


hitOrStand :: (PlayerCards, DealerFaceUp) -> (EV, Action)
hitOrStand boardState = max (calculateStandEV boardState, Stand) (calculateHitEV boardState, Hit)

--Stand EV is the root of all player actions, if we assign "bust" or "losing" a value of 0,
--we don't need to calculate the odds of such, and when we do need bust / loss probability
--we can simply subtract.

calculateHitEV :: (PlayerCards, DealerFaceUp) -> EV
calculateHitEV input@(playerCards, dealerFaceUp) =
    maximum (probabilityOfPlayerDrawOnStandEV <$> appendPlayerCard input) --this is a mess.


probabilityOfPlayerDrawOnStandEV :: (PlayerCards, DealerFaceUp) -> EV
probabilityOfPlayerDrawOnStandEV boardState@(playerCards, dealerFaceUp) =
    let cardsAlreadyDrawn = playerCards `snoc` dealerFaceUp in
    calculateStandEV boardState *
    (
        fromIntegral
        (
            16 -
            length
            (filter (== last playerCards) $ init cardsAlreadyDrawn)
        )
        /
        fromIntegral
        (
            416 +
            1 -
            length cardsAlreadyDrawn
        )
    )
    


appendPlayerCard :: (PlayerCards, DealerFaceUp) -> Vector (PlayerCards, DealerFaceUp)
appendPlayerCard (playerCards, dealerFaceUp) =
    do
        newCard <- twoToAce
        if 21 < convertToValueHand (playerCards `snoc` newCard)
            then empty
            else pure (playerCards `snoc` newCard, dealerFaceUp)


calculateStandEV :: (PlayerCards, DealerFaceUp) -> EV
calculateStandEV boardState@(playerCards, dealerFaceUp) =
    case playerCards of
        cards 
            | elem cards $
            pure (pure Ace `snoc` TenJackQueenKing) `snoc`
            (pure TenJackQueenKing `snoc` Ace) ->
                undefined
            | length cards == 6 ->
                undefined
            | 21 == convertToValueHand cards ->
                undefined
            | otherwise ->
                undefined


convertToValueHand :: Vector Card -> Int
convertToValueHand dealerCards =
    go dealerCards 0 0
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


appendCardDealer :: Card -> Vector DealerHand
appendCardDealer dealerFaceUp =
    appendCard =<<
    appendCard =<<
    appendCard =<<
    appendCard =<< basicHand dealerFaceUp
  where
    basicHand :: Card -> Vector DealerHand
    basicHand dealerFaceUp =
        snoc (pure dealerFaceUp) <$> twoToAce
    appendCard :: DealerHand -> Vector DealerHand
    appendCard hand =
        if 17 <= convertToValueHand hand
            then pure hand
            else do
                newCard <- twoToAce
                if 21 < convertToValueHand (snoc (pure dealerFaceUp) newCard) ||
                    newCard < last hand
                    then empty
                    else pure $ snoc hand newCard


calculateEV :: (PlayerCards, Card) -> DealerHand -> EV
calculateEV (uncurry snoc -> cardsInPlay) dealerHand =
    calculateProbability cardsInPlay (tail dealerHand) 1 *
    numberOfPermutationsDealerHand (tail dealerHand)
  where
    calculateProbability :: Vector Card -> DealerHand -> Double -> EV
    calculateProbability cardsInPlay (null -> True) storedValue = storedValue
    calculateProbability cardsInPlay dealerHand storedValue =
        case dealerHand!0 of
            TenJackQueenKing ->
                calculateProbability
                (cardsInPlay `snoc` (dealerHand!0))
                (tail dealerHand) $
                (
                    128 -
                    fromIntegral
                    (
                        length $
                        filter (==TenJackQueenKing) cardsInPlay
                    )
                    /
                    416 - fromIntegral (length cardsInPlay)
                ) * storedValue
            other ->
                calculateProbability
                (cardsInPlay `snoc` (dealerHand!0))
                (tail dealerHand) $
                (
                    32 -
                    fromIntegral
                    (
                        length $
                        filter (==other) cardsInPlay
                    )
                    /
                    416 - fromIntegral (length cardsInPlay)
                ) * storedValue

    numberOfPermutationsDealerHand :: Vector Card -> Double
    numberOfPermutationsDealerHand dealerHand =
        let lengthOfHand = length dealerHand
            numberOfSingles = 0 --should be undefined
            numberOfDoubles = 0 --should be undefined
            aceCase = 0 in --should be undefinedin
        fromIntegral $
                fac lengthOfHand -
                (
                    numberOfSingles *
                    fac (lengthOfHand - 1) +
                    numberOfDoubles *
                    fac (lengthOfHand - 2) +
                    aceCase
                )
         -- needs revision, this is a placeholder to get this to compile

fac :: Int -> Int
fac = facAcc 1
  where
    facAcc acc 1 = acc
    facAcc acc n = facAcc (acc*n) (n-1)