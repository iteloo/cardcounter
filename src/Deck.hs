{-# LANGUAGE NoMonomorphismRestriction #-}

module Deck (
      Card(..)
    , Hand
    , Deck
    , RawPoint
    , Point(..)

    , maxRawPoint
    , fromRawPoint
    , points
    , sumPoints

    , BareHandData(..)
    , HandAttr(..)
    , FactorOfReturn
    , addBHD
    , bareData
    , attribute
    , attr
    , factorOfReturn
    , factorOfReturn'
    , normalHand
    , normalBareData

    , compareHands
    , compareBareData

    , fullDeck
    , tally
    , addCard
    , removeCard
  ) where

import Helpers

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe as Maybe


-- Basic data types

data Card = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10
    | J | Q | K | Joker
  deriving (Ord, Eq, Read, Show, Enum)

type Hand = [Card]

type Deck = Map Card Int

type RawPoint = Rational

data Point = Stinky | Safe RawPoint deriving (Eq, Ord)

instance Show Point where
  show (Safe x) = show $ (fromRational x :: Float)
  show Stinky = "Stinky"

instance Monoid Point where
  mempty = Safe 0

  Stinky `mappend` _ = Stinky
  _ `mappend` Stinky = Stinky
  Safe x `mappend` Safe y =
    let z = x + y
    in if z <= maxRawPoint then Safe z else Stinky

maxRawPoint = 10.5 :: RawPoint

fromRawPoint x = if x > maxRawPoint then Stinky else Safe x

points c = Safe $ case c of
  N1 -> 1
  N2 -> 2
  N3 -> 3
  N4 -> 4
  N5 -> 5
  N6 -> 6
  N7 -> 7
  N8 -> 8
  N9 -> 9
  N10 -> 10
  _ -> 0.5

sumPoints :: [Card] -> Point
sumPoints = foldMap points

-- Hand Data

data BareHandData = BHD { size :: Int, totalPoints :: Point }
    deriving (Ord, Eq)

data HandAttr = HandAttr { fiveCards :: Bool, tenAndHalf :: Bool }
    deriving (Show, Ord, Eq)

type FactorOfReturn = Int

addBHD bhd1 bhd2 = BHD {
    size = size bhd1 + size bhd2,
    totalPoints = totalPoints bhd1 `mappend` totalPoints bhd2
  }

bareData :: Hand -> BareHandData
bareData h = BHD { size = length h, totalPoints = sumPoints h }

attr :: BareHandData -> HandAttr
attr d =
  let p = totalPoints d
  in HandAttr {
      fiveCards = and [p /= Stinky, size d == 5],
      tenAndHalf = p == Safe 10.5
    }

attribute :: Hand -> HandAttr
attribute = attr . bareData

factorOfReturn :: Hand -> FactorOfReturn
factorOfReturn = factorOfReturn' . bareData

factorOfReturn' :: BareHandData -> FactorOfReturn
factorOfReturn' bhd =
  let a = attr bhd
      tah = tenAndHalf a
      fc = fiveCards a
  in if and [tah, fc]
      then 5
      else if fc
        then 3
        else if tah
          then 2
          else 0

normalHand :: Hand -> Bool
normalHand = normalBareData . bareData

normalBareData :: BareHandData -> Bool
normalBareData bhd = attr bhd == HandAttr False False

-- Hand operations

compareHands :: Hand -> Hand -> FactorOfReturn
compareHands h1 h2 =
  if and [normalHand h1, normalHand h2]
    then if sumPoints h1 > sumPoints h2
      then 1
      else if sumPoints h1 == sumPoints h2
        then 0
        else -1
    else factorOfReturn h1 - factorOfReturn h2

compareBareData :: BareHandData -> BareHandData -> FactorOfReturn
compareBareData bhd1 bhd2 =
  if and [normalBareData bhd1, normalBareData bhd2]
    then if totalPoints bhd1 > totalPoints bhd2
      then 1
      else if totalPoints bhd1 == totalPoints bhd2
        then 0
        else -1
    else factorOfReturn' bhd1 - factorOfReturn' bhd2

-- Deck operations

fullDeck = Map.fromList $ [(c, 4) | c <- [N1 .. K]] ++ [(Joker, 2)]

tally :: [Card] -> Deck
tally cs = Map.fromListWith (+) . zip cs $ repeat 1

addCard :: Card -> Deck -> Deck
addCard = Map.alter (Just . (+1) . Maybe.fromMaybe 0)

removeCard :: Card -> Deck -> Either String Deck
removeCard c deck =
  let n = Maybe.fromMaybe 0 $ Map.lookup c deck
  in maybeToEither
    ("Error: No " ++ show c ++ " in deck/hand to remove!")
    $ ifToMaybe (n > 0) $ Map.insert c (n-1) deck
