{-# LANGUAGE LambdaCase, BangPatterns #-}

module CommonNamesAndFunctions where

import DataDeclarations
    ( Probability, CardsInPlay, Card(Ten_Jack_Queen_King, Two, Ace) )
import CardValueChecker

import Control.Applicative ((<**>))
import Data.List (intersect)



-- | A list of cards that comprise a rank, or all cards of
-- the same suit. Note absence of ReducedAce.

allRanks :: [Card]
allRanks = [ Two .. Ace ]





-- | Hands that are "naturals"

natural :: [[Card]]
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



-- Used both in dealer and player sides, easier to move this out of a where block.





numberOfCardsIn :: Card -> [Card] -> Double
numberOfCardsIn specificCard =
      
    fromIntegral . length . filter (==specificCard)

probabilityTenJackQueenKing :: [Card] -> Double
probabilityTenJackQueenKing !cardsInPlay =
    ( 128 - numberOfCardsIn Ten_Jack_Queen_King cardsInPlay ) /
    (416 - fromIntegral (length cardsInPlay) )

probabilityOther :: [Card] -> Card -> Double
probabilityOther !cardsInPlay !other =
    ( 32 - numberOfCardsIn other cardsInPlay ) /
    (416 - fromIntegral (length cardsInPlay) )

--AppendNewCardPlayer has been moved out from hit vs stand decision section, since it's going to be
--reused by the double and split functions.

appendNewCardPlayer :: CardsInPlay -> [ CardsInPlay ]
appendNewCardPlayer cardsInPlay@( playerCards , dealerFaceUp ) =
    fmap (flip (,) dealerFaceUp) . filter (valueCheck 21 (>=)) $
    (pure playerCards) <**> (flip (++) <$> pure <$> allRanks )

probabilityOfPlayerDraw :: CardsInPlay -> Probability
probabilityOfPlayerDraw cardsInPlay@(playerCards, dealerCards) =
    let combinedCardsInPlay = safeInitThroughNil playerCards ++ dealerCards in
    case safeLastThroughNil playerCards of
        [] -> 1
        [Ten_Jack_Queen_King] -> probabilityTenJackQueenKing combinedCardsInPlay
        [otherCard] -> probabilityOther combinedCardsInPlay otherCard
