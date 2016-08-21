module Helpers (
    toMaybe
  , ifToMaybe
  , maybeToEither
) where

import Data.Maybe as Maybe


toMaybe cond x = listToMaybe $ filter cond $ [x]

ifToMaybe b x = if b then Just x else Nothing

maybeToEither e m = case m of
  Just x -> Right x
  Nothing -> Left e
