{-# LANGUAGE LambdaCase, BangPatterns, OverloadedLists #-}

module CommonNamesAndFunctions where

import DataDeclarations ( Probability, CardsInPlay, Card(..) )
import CardValueChecker

import Control.Applicative ((<**>))
import Data.List (intersect)
import Data.Set (Set)
import Control.Applicative ((<**>))
import Data.Vector



-- | A list of cards that comprise a rank, or all cards of
-- the same suit.

allRanks :: [Card]
allRanks = [ Two .. Ace ]

-- | Hands that are "naturals"

natural :: [[Card]]
natural = [ [ Ace , Ten_Jack_Queen_King ] , [ Ten_Jack_Queen_King , Ace ] ]

naturalSet :: Set (Card, Vector Card)
naturalSet = [(Ace, [Ten_Jack_Queen_King]),(Ten_Jack_Queen_King,[Ace])]

-- Some safe versions of list functions, written with nil as a default value

safeTailThroughNil :: [a] -> [a]
safeTailThroughNil =
  \case
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

-- Used both in dealer and player sides, easier to move this out of a where block.

numberOfCardsIn :: Card -> [Card] -> Int
numberOfCardsIn specificCard =
    
    Prelude.length .
    Prelude.filter (==specificCard)

probabilityTenJackQueenKing :: [Card] -> Double
probabilityTenJackQueenKing cardsInPlay =
    fromIntegral
    (
        128 - 
        numberOfCardsIn Ten_Jack_Queen_King cardsInPlay
    )
        /
    fromIntegral
    (
        416 -
        Prelude.length cardsInPlay
    )

probabilityOther :: [Card] -> Card -> Double
probabilityOther cardsInPlay other =
    fromIntegral
    (
        32 - 
        numberOfCardsIn other cardsInPlay
    )
        /
    fromIntegral
    (
        416 -
        Prelude.length cardsInPlay
    )

--AppendNewCardPlayer has been moved out from hit vs stand decision section, since it's going to be
--reused by the double and split functions.

appendNewCardPlayer :: CardsInPlay -> [CardsInPlay]
appendNewCardPlayer cardsInPlay@( playerCards , dealerFaceUp ) =
    fmap (flip (,) dealerFaceUp) .
    Prelude.filter (valueCheck 21 (>=)) $
    [playerCards] <**>
    (
        (flip (<>).pure) <$>
        allRanks
    )

--Next function is used both in top-level evaluator and hitstand evaluator.

probabilityOfPlayerDraw :: CardsInPlay -> Probability
probabilityOfPlayerDraw cardsInPlay@(playerCards, dealerCards) =
    let combinedCardsInPlay = safeInitThroughNil playerCards <> dealerCards in
    case safeLastThroughNil playerCards of
        [] -> 1
        [Ten_Jack_Queen_King] -> probabilityTenJackQueenKing combinedCardsInPlay
        [otherCard] -> probabilityOther combinedCardsInPlay otherCard
