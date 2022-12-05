{-# LANGUAGE LambdaCase #-}
module CalculateHandValue (checkForSoft17, checkIfBust, handValueOf) where


import Data.Vector (Vector)
import qualified Data.Vector as Vec
import CalculateTypes (Card (..))


-- | Checks for soft 17. Somewhat wasteful, but makes things explicit in code.
-- Previously considered modifying to just force Ace to 11,
-- but that caused error cases wherein hands were compressed to
-- 14 or so in order to avoid a bust, but this triggered
-- True when it should be false due to compression.

checkForSoft17 :: Vector Card -> Bool
checkForSoft17 hand =
    17 <= handValueOf hand

-- | Checks if a hand has gone bust.

checkIfBust :: Vector Card -> Bool
checkIfBust hand =
    21 < sum (modifiedFromEnum <$> hand)
  where
    modifiedFromEnum :: Card -> Int
    modifiedFromEnum = \case
        Ace -> 1
        card -> fromEnum card

-- | Converts hands to highest non-bust value of hand.

handValueOf :: Vector Card -> Int
handValueOf hand =
    let rawValue = Vec.sum $ fromEnum <$> hand 
        aceCount = Vec.length $ Vec.filter (==Ace) hand
    in downConvert rawValue aceCount
  where
    downConvert :: Int -> Int -> Int
    downConvert rawValue 0 = rawValue
    downConvert rawValue n
        | rawValue > 21 =
            downConvert (rawValue-10) (n-1)
        | otherwise = rawValue