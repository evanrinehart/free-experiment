{-# LANGUAGE BangPatterns #-}
module Kahan where

import Data.List

data Kahan = Kah !Double !Double deriving (Show, Read)

zero :: Kahan
zero = Kah 0 0

accum :: Kahan -> Double -> Kahan
accum (Kah a c) x =
  let !y = x - c in
  let !t = a + y in
  Kah t ((t - a) - y)

extract :: Kahan -> Double
extract (Kah a c) = a

ksum :: [Double] -> Double
ksum xs = extract (foldl' accum zero xs)
