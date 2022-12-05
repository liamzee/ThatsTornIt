{-# LANGUAGE TupleSections, ImportQualifiedPost #-}

module SplitOutputter where

import CalculateDealerHands (countedDealerHandsIncludingBust)
import CalculateHandValue (checkIfBust, handValueOf)
import CalculateNonSplitBoardStates (allNonSplitBoardStates, allPlayerHands)
import CalculateProbabilityOfHand (calculateOddsOf)
import CalculateStand (isNatural)
import CalculateTwoToAce (twoToAce, twoToAceSet)
import CalculateTypes (Action (DoubleAction, Hit, Split, Stand), BoardState, Card (..), EV, EVAction, PlayerCards)
import Control.Applicative (liftA3)
import Control.Arrow ((&&&))
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.List qualified
import Data.Map (Map)
import Data.Map qualified as LazyMap
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector, cons, snoc)
import Data.Vector qualified as Vec
import Debug.Trace (traceShowId)
import EvaluateSplit (parallelizeMapping)
import JSONTypes (Entries (Entry), Splits (..))
import Parallelize (parallelizeLazySplitEVAction)

packedJSONSplits :: Map (Card, Card, Vector Card) EV -> ByteString
packedJSONSplits = encode . responseListSplits

responseListSplits :: Map (Card, Card, Vector Card) EV -> Vector Splits
responseListSplits mapping = makeSplits <$> listOfSplitStates
  where
    makeSplits :: (Card, Card, Card) -> Splits
    makeSplits (dealerCard, playerSplitCard, newCard) =
      Splits
        dealerCard
        playerSplitCard
        (pure playerSplitCard `snoc` newCard)
        (evaluateDoubleHitStand (pure playerSplitCard `snoc` newCard))
        $ Set.toList . Set.map makeEntries
        $ reducedNonSplitBoardStates
      where
        makeEntries :: BoardState -> Entries
        makeEntries boardState@(playerCards, dealerFaceUp) =
          Entry dealerFaceUp playerCards $ evaluateHitOrStand playerCards

        reducedNonSplitBoardStates :: Set BoardState
        reducedNonSplitBoardStates =
          Set.filter
            reducedNonSplitBoardStatesFilter
            allNonSplitBoardStates
          where
            reducedNonSplitBoardStatesFilter (pCards, dFaceUp) =
              if newCard /= playerSplitCard
                then Vec.elem newCard pCards && Vec.elem playerSplitCard pCards && dealerCard == dFaceUp
                else 2 <= length (Vec.filter (== newCard) pCards) && dealerCard == dFaceUp

        evaluateDoubleHitStand :: PlayerCards -> EVAction
        evaluateDoubleHitStand playerCards =
          max (evaluateDoubleFirstSplit playerCards, DoubleAction) (evaluateHitOrStand playerCards)
          where
            evaluateDoubleFirstSplit :: PlayerCards -> EV -- looks like you got confused by the generate possibilities crap, then calculate odds of such
            evaluateDoubleFirstSplit playerCards =
              Vec.sum $
                uncurry (*)
                  . (calculateOddsOf ((pure playerSplitCard `snoc` dealerCard) <> Vec.init playerCards) . Vec.drop 2 &&& bustCarrier calculateStandDouble)
                  <$> (snoc playerCards <$> twoToAce) -- add another card to a vector of hands, modeling dealer stand.
              where
                bustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
                bustCarrier function playerHand =
                  if checkIfBust playerHand
                    then bustEVCallDouble playerHand
                    else function playerHand

                bustEVCallDouble :: PlayerCards -> EV
                bustEVCallDouble playerHand =
                  subtract 2 . Vec.sum $ -- subtract 2 to model player losing double the money
                    uncurry (*)
                      . ( calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerCard)) . Vec.drop 1
                            &&& (\u -> fromMaybe (error $ show u <> "bustEVCallDouble") (mapping LazyMap.!? u))
                              . (playerSplitCard,dealerCard,)
                              . Vec.fromList
                              . Data.List.sort
                              . Vec.toList
                              . (<> playerHand)
                        )
                      <$> possibleDealerHands
                  where
                    possibleDealerHands :: Vector (Vector Card)
                    possibleDealerHands = cons dealerCard . pure <$> twoToAce

            calculateStandDouble :: PlayerCards -> EV
            calculateStandDouble = (\u -> fromMaybe (error $ show u <> "calculateStandDouble") (parallelizeMapping allPlayerHandsStandDouble calculateStandDouble' LazyMap.!? u)) . Vec.fromList . Data.List.sort . Vec.toList

            allPlayerHandsStandDouble :: Set (Vector Card)
            allPlayerHandsStandDouble = Set.unions $ Set.map appendAllStands twoToAceSet
              where
                appendAllStands :: Card -> Set (Vector Card)
                appendAllStands card = Set.map (Vec.fromList . Data.List.sort . Vec.toList . (`snoc` card)) $ Set.fromList $ Vec.toList allPlayerHandsStartFirstSplit

            allPlayerHandsStartFirstSplit :: Vector (Vector Card)
            allPlayerHandsStartFirstSplit = snoc (pure playerSplitCard) <$> twoToAce

            -- up to here right now, ugh
            calculateStandDouble' :: Vector Card -> EV
            calculateStandDouble' playerHand =
              Vec.sum $
                uncurry (*)
                  . ( uncurry (*)
                        . ( calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerCard)) . Vec.drop 1 . fst
                              &&& fromIntegral . snd
                          )
                        &&& evaluateHandValue . fst
                    )
                  <$> applicableDealerHands
              where
                applicableDealerHands :: Vector (Vector Card, Int)
                applicableDealerHands
                  | checkIfBust playerHand = (,1) . cons dealerCard . pure <$> twoToAce
                  | otherwise = Vec.filter ((== dealerCard) . Vec.head . fst) countedDealerHandsIncludingBust

                evaluateHandValue :: Vector Card -> EV
                evaluateHandValue item =
                  winLossDraw + examiner
                  where
                    examiner = (\u -> fromMaybe (error $ show u <> "calculateStandDoubleEvaluate") (mapping LazyMap.!? u)) (playerSplitCard, dealerCard, Vec.fromList . Data.List.sort . Vec.toList $ (playerHand <> item))
                    winLossDraw
                      | isNatural item = -2
                      | checkIfBust item = 2
                      | 6 == length item = -2
                      | handValueOf playerHand > handValueOf item = 2
                      | handValueOf playerHand == handValueOf item = 0
                      | handValueOf playerHand < handValueOf item = -2

        reducedPlayerHands :: Set (Vector Card)
        reducedPlayerHands = Set.filter (\u -> elem playerSplitCard u && elem newCard u) allPlayerHands

        evaluateHitOrStand :: PlayerCards -> EVAction
        evaluateHitOrStand = (\u -> fromMaybe (error $ show u <> "evaluateHitOrStand" <> "playerCard" <> show playerSplitCard <> "newCard" <> show newCard <> "dealercard" <> show dealerCard) (parallelizeLazySplitEVAction reducedPlayerHands evaluateHitOrStand' LazyMap.!? u)) . Vec.fromList . Data.List.sort . Vec.toList
          where
            evaluateHitOrStand' :: PlayerCards -> EVAction
            evaluateHitOrStand' playerCards
              | 6 == length playerCards = (calculateStand playerCards, Stand)
              | 21 == handValueOf playerCards = (calculateStand playerCards, Stand)
              | otherwise =
                  max (calculateStand playerCards, Stand) $
                    ( Vec.sum $
                        uncurry (*)
                          . ( (calculateOddsOf (playerCards <> (pure playerSplitCard `snoc` dealerCard)) . pure . Vec.last)
                                &&& bustCarrier (fst . evaluateHitOrStand)
                            )
                          <$> (snoc playerCards <$> twoToAce),
                      Hit
                    )
              where
                bustCarrier :: (PlayerCards -> EV) -> PlayerCards -> EV
                bustCarrier function playerHand =
                  if checkIfBust playerHand
                    then bustEVCall playerHand
                    else function playerHand

                bustEVCall :: PlayerCards -> EV
                bustEVCall playerHand =
                  subtract 1 . Vec.sum $
                    uncurry (*)
                      . ( calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerCard)) . Vec.drop 1
                            &&& (\u -> fromMaybe (error $ show u <> "bustEVCall evaluateHitOrStand") ((LazyMap.!?) mapping u))
                              . (playerSplitCard,dealerCard,)
                              . Vec.fromList
                              . Data.List.sort
                              . Vec.toList
                              . (<> playerHand)
                        )
                      <$> possibleDealerHands
                  where
                    possibleDealerHands :: Vector (Vector Card)
                    possibleDealerHands = cons dealerCard . pure <$> twoToAce

            calculateStand :: PlayerCards -> EV
            calculateStand = (\u -> fromMaybe (error $ show u <> "calculateStand" <> "playerCard:" <> show playerSplitCard <> "dealerFaceUp" <> show dealerCard) (parallelizeMapping reducedPlayerHands calculateStand' LazyMap.!? u)) . Vec.fromList . Data.List.sort . Vec.toList
              where
                calculateStand' :: PlayerCards -> EV
                calculateStand' playerHand =
                  Vec.sum $
                    uncurry (*)
                      . ( uncurry (*)
                            . ( calculateOddsOf (playerHand <> (pure playerSplitCard `snoc` dealerCard)) . Vec.drop 1 . fst
                                  &&& fromIntegral . snd
                              )
                            &&& evaluateHandValue . fst
                        )
                      <$> Vec.filter ((== dealerCard) . Vec.head . fst) countedDealerHandsIncludingBust
                  where
                    evaluateHandValue :: Vector Card -> EV
                    evaluateHandValue item =
                      winLossDraw
                        + (\u -> fromMaybe (error $ show u <> "evaluateHandvalue") $ mapping LazyMap.!? u)
                          ( playerSplitCard,
                            dealerCard,
                            Vec.fromList . Data.List.sort . Vec.toList $
                              playerHand <> item
                          )
                      where
                        winLossDraw
                          | isNatural playerHand && not (isNatural item) = 1.5
                          | isNatural playerHand && isNatural item = 0
                          | isNatural item = -1
                          | checkIfBust item = 1
                          | 6 == length playerHand && 6 /= length item = 1
                          | 6 == length playerHand && handValueOf playerHand > handValueOf item = 1
                          | 6 == length playerHand && handValueOf playerHand == handValueOf item = 0
                          | 6 == length item = -1
                          | handValueOf playerHand > handValueOf item = 1
                          | handValueOf playerHand == handValueOf item = 0
                          | handValueOf playerHand < handValueOf item = -1

listOfSplitStates :: Vector (Card, Card, Card)
listOfSplitStates = liftA3 (,,) twoToAce twoToAce twoToAce