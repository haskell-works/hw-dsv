{-# LANGUAGE TupleSections #-}

module App.Data.List
  ( splitOn
  ) where

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] =  []
splitOn a xs = cons $ case break (== a) xs of
  (l, ys) -> (l, ) $ case ys of
    []   -> []
    _:zs -> splitOn a zs
  where cons ~(h, t) =  h : t
