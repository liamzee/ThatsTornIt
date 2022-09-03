module CalculateHandValue where


import Data.Vector
import qualified Data.Vector as Vec
import CalculateTypes (Card (..))


fromEnumCard :: Card -> Int
fromEnumCard = (+2). fromEnum


checkForSoft17 :: Vector Card -> Bool
checkForSoft17 hand =
    17 <= handValueOf hand


checkIfBust :: Vector Card -> Bool
checkIfBust hand =
    (21 <) . Vec.sum $ modifiedFromEnum <$> hand
  where
    modifiedFromEnum :: Card -> Int
    modifiedFromEnum card
        | Ace == card =
            1
        | otherwise =
            fromEnumCard card


handValueOf :: Vector Card -> Int
handValueOf hand =
    let rawValue = Vec.sum $ fromEnumCard <$> hand 
        aceCount = Vec.length $ Vec.filter (==Ace) hand in
    downConvert rawValue aceCount
  where
    downConvert rawValue 0 = rawValue
    downConvert rawValue n
        | rawValue > 21 =
            downConvert (rawValue-10) (n-1)
        | otherwise = rawValue