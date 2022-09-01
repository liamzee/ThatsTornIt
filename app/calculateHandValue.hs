module CalculateHandValue where


import Data.Sequence (Seq(..), filter)
import qualified Data.Sequence as Sequ
import CalculateTypes (Card (..))


fromEnumCard :: Card -> Int
fromEnumCard = (+2). fromEnum


checkForSoft17 :: Seq Card -> Bool
checkForSoft17 hand =
    17 <= handValueOf hand


checkIfBust :: Seq Card -> Bool
checkIfBust hand =
    (21 <) . sum $ modifiedFromEnum <$> hand
  where
    modifiedFromEnum :: Card -> Int
    modifiedFromEnum card =
        if Ace == card
            then 1
            else fromEnumCard card


handValueOf :: Seq Card -> Int
handValueOf hand =
    let rawValue = sum $ fromEnumCard <$> hand 
        aceCount = length $ Sequ.filter (==Ace) hand in
    downConvert rawValue aceCount
  where
    downConvert rawValue 0 = rawValue
    downConvert rawValue n =
        if rawValue > 21
            then downConvert (rawValue-10) (n-1)
            else rawValue