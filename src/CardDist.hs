
module CardDist (
      draw
    , shouldAdd1
    , shouldAdd1Full
  ) where

import Dist
import Deck

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad


draw :: Deck -> Dst Card
draw deck =
  let size = Map.foldr (+) 0 deck
  in categorical $ Map.toList $
    Map.map ((/ (fromIntegral size)) . fromIntegral) deck

drawUniform = draw fullDeck

drawMany :: Int -> Dst [Card]
drawMany n = sequence $ take n $ repeat drawUniform

drawAndSum' :: Int -> Dst Point
drawAndSum' n = do
  cs <- drawMany n
  return $ sumPoints cs

drawAndSum :: Int -> Dst Point
drawAndSum n =
  let acc c total = total `mappend` (points c)
      reifAcc c total = reify $ liftM2 acc c total
  in foldr reifAcc (return mempty) $ take n $ repeat drawUniform

minCatch = Safe 7

dealerDraw' :: Dst Hand
dealerDraw' =
  let dealerDraw cs = do
        if or [length cs == 5, sumPoints cs >= minCatch]
          then return cs
          else do
            c <- drawUniform
            dealerDraw $ c:cs
  in dealerDraw []

dealerDraw :: Deck -> Dst BareHandData
dealerDraw deck =
  let dealerDraw' bhd = reify $ do
        if or [size bhd == 5, totalPoints bhd >= minCatch]
          then return bhd
          else do
            c <- draw deck
            dealerDraw' $ addBHD bhd $ bareData [c]
  in dealerDraw' $ bareData []

dealSum :: Dst Point
dealSum =
  let dealSum' p n = do
        if or [n == 5, p >= minCatch]
          then return p
          else do
            c <- drawUniform
            dealSum' (p `mappend` (points c)) $ n+1
  in dealSum' (Safe 0) 0

add1 :: Hand -> Dst Point
add1 h = do
  c <- drawUniform
  return $ sumPoints $ c:h

diffAdd :: Hand -> Dst FactorOfReturn
diffAdd h = do
  dh <- dealerDraw'
  c <- drawUniform
  let noAdd = compareHands h dh
  let add = compareHands (c:h) dh
  return $ add - noAdd

diffAdd' :: Deck -> BareHandData -> Dst FactorOfReturn
diffAdd' deck bhd = do
  d_bhd <- dealerDraw deck
  c <- draw deck
  let noAdd = compareBareData bhd d_bhd
  let add = compareBareData (bareData [c] `addBHD` bhd) d_bhd
  return $ add - noAdd

hitMore :: Deck -> BareHandData -> Dst BareHandData
hitMore deck bhd = do
  if totalPoints bhd /= Stinky
    then do
      c <- draw deck
      return $ bareData [c] `addBHD` bhd
    else
      return bhd

shouldAdd1 :: Deck -> Hand -> Bool
shouldAdd1 deck = fst . shouldAdd1Full deck

shouldAdd1Full :: Deck -> Hand -> (Bool, Double)
shouldAdd1Full deck h =
  let diff = mean $ diffAdd' deck $ bareData h
  in (diff > 0, diff)
