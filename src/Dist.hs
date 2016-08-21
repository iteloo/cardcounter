{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module Dist (
      Prob
    , Dist

    , PT
    , normalize

    , Dst  -- (constructors private)

    , runExact
    , categorical
    , reify
    , mean
  ) where

import Data.Map (Map)
import qualified Data.Map as Map


type Prob = Double

class Monad d => Dist d where
  bern :: Prob -> d Bool
  failure :: d b

-- Imp of Dist

type PT a = [(a,Prob)]  -- Ord a

normalize :: PT a -> PT a
normalize pt = map (map_snd (/nf)) pt
 where nf = sum $ map snd pt

-- Dst: impl of Dist (constructors private)

data Dst a where
  Single :: PT a -> Dst a
  Chain  :: PT b -> (b -> Dst a) -> Dst a

instance Functor Dst where
   fmap f (Single pt) = Single (map (map_fst f) pt)
   fmap f (Chain d k) = Chain d (fmap f . k)

instance Applicative Dst where
  pure    = return
  m <*> x = do {mv <- m; xv <- x; return (mv xv)}

instance Monad Dst where
  return x = Single [(x,1)]

  Single []      >>= k = Single []
  Single [(x,1)] >>= k = k x    -- Nothing to sum over
  Single pt      >>= k = Chain pt k
  Chain pt k1    >>= k = Chain pt (\x -> k1 x >>= k)

instance Dist Dst where
  bern p  = Single [(True, p), (False,1-p)]
  failure = Single []

-- Dst operations: Exact inference

runExact :: Ord a => Dst a -> PT a
runExact (Single x) = x
runExact (Chain pt k) =
  Map.toList . Map.fromListWith (+) .
   concatMap (\(x,p) -> map (map_snd (*p)) $ runExact (k x)) $ pt

categorical :: PT a -> Dst a
categorical = Single

reify = categorical . runExact

mean :: (Ord a, Real a) => Dst a -> Double
mean d = sum $ fmap (\(v,p) -> (toDouble v)*p) $ runExact d

-- helpers

map_fst :: (a -> b) -> (a,c) -> (b,c)
map_fst f (x,y) = (f x,y)

map_snd :: (a -> b) -> (c,a) -> (c,b)
map_snd f (x,y) = (x,f y)

toDouble :: Real a => a -> Double
toDouble = fromRational . toRational
