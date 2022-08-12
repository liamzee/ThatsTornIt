{-# LANGUAGE LambdaCase, BangPatterns, OverloadedLists #-}

module CommonNamesAndFunctions where

import DataDeclarations ( Probability, CardsInPlay, Card(..) )
import CardValueChecker

import Control.Applicative ((<**>))
import Data.List (intersect)
import Data.Set (Set)
import Data.Sequence
import qualified Data.Sequence as Seq
import Control.Applicative ((<**>))



-- | A list of cards that comprise a rank, or all cards of
-- the same suit. Note absence of ReducedAce.

allRanks :: [Card]
allRanks = [ Two .. Ace ]

allRanksNested :: Seq [Card]
allRanksNested = 
    [Two] :<|
    [Three] :<|
    [Four] :<|
    [Five] :<|
    [Six] :<|
    [Seven] :<|
    [Eight] :<|
    [Nine] :<|
    [Ten_Jack_Queen_King] :<|
    Empty

-- | Hands that are "naturals"

natural :: Seq [Card]
natural = [ [ Ace , Ten_Jack_Queen_King ] , [ Ten_Jack_Queen_King , Ace ] ]





-- Some safe versions of list functions, written with nil as a default value

safeTailThroughNil :: [a] -> [a]
safeTailThroughNil = \case

    [] -> []
    x:xs -> xs

safeInitThroughNil :: [a] -> [a]
safeInitThroughNil = \case

    [] -> []
    [x] -> []
    x:xs -> x : safeInitThroughNil xs

safeLastThroughNil :: [a] -> [a]
safeLastThroughNil = \case

    [] -> []
    [x] -> [x]
    x:xs -> safeLastThroughNil xs

seqIsPrefixOf :: Eq a => a -> Seq a -> Bool
seqIsPrefixOf _ Empty = False
seqIsPrefixOf elementToBeChecked (x:<|xs) | elementToBeChecked == x = True
   | otherwise = False



-- Used both in dealer and player sides, easier to move this out of a where block.





numberOfCardsIn :: Card -> [Card] -> Double
numberOfCardsIn specificCard =
    fromIntegral .
    Prelude.length .
    Prelude.filter (==specificCard)

probabilityTenJackQueenKing :: [Card] -> Double
probabilityTenJackQueenKing cardsInPlay =
    (
        128 - 
        numberOfCardsIn Ten_Jack_Queen_King cardsInPlay
    )
    /
    (
        416 -
        (fromIntegral $ Prelude.length cardsInPlay)
    )


probabilityOther :: [Card] -> Card -> Double
probabilityOther cardsInPlay other =
    (
        32 - 
        numberOfCardsIn other cardsInPlay
    )
    /
    (
        416 -
        (fromIntegral $ Prelude.length cardsInPlay)
    )





--AppendNewCardPlayer has been moved out from hit vs stand decision section, since it's going to be
--reused by the double and split functions.

appendNewCardPlayer :: CardsInPlay -> Seq CardsInPlay
appendNewCardPlayer cardsInPlay@( playerCards , dealerFaceUp ) =
    fmap (flip (,) dealerFaceUp) .
    Seq.filter (valueCheck 21 (>=)) $
    ((pure playerCards)) <**>
    (flip (<>) <$> allRanksNested )


probabilityOfPlayerDraw :: CardsInPlay -> Probability
probabilityOfPlayerDraw cardsInPlay@(playerCards, dealerCards) =
    let combinedCardsInPlay = safeInitThroughNil playerCards <> dealerCards in
    case safeLastThroughNil playerCards of
        [] -> 1
        [Ten_Jack_Queen_King] -> probabilityTenJackQueenKing combinedCardsInPlay
        [otherCard] -> (probabilityOther combinedCardsInPlay otherCard)
