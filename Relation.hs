module Relation where

import Data.Map
import qualified Data.List as L
import Prelude hiding (delete, lookup)
import Data.Maybe

data Relation a b = Rel (Map a [b]) (Map b [a]) deriving (Show)

empty :: Relation a b
empty = Rel Data.Map.empty Data.Map.empty

insert :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insert x y (Rel m1 m2) = Rel m1' m2' where
  m1' = insertWith (++) x [y] m1
  m2' = insertWith (++) y [x] m2

lookupL :: Ord a => a -> Relation a b -> [b]
lookupL k (Rel m1 _) = fromMaybe [] (lookup k m1)

lookupR :: Ord b => b -> Relation a b -> [a]
lookupR k (Rel _ m2) = fromMaybe [] (lookup k m2)

lookupL1 :: Ord a => a -> Relation a b -> Maybe b
lookupL1 k (Rel m1 _) = lookup k m1 >>= listToMaybe

minL :: Relation a b -> Maybe (a, [b])
minL (Rel m1 m2) = fmap fst (minViewWithKey m1)

minR :: Relation a b -> Maybe (b, [a])
minR (Rel m1 m2) = fmap fst (minViewWithKey m2)

remove :: Eq a => a -> [a] -> Maybe [a]
remove x xs = case L.filter (/= x) xs of
  [] -> Nothing
  xs -> Just xs

deleteR :: (Ord a, Ord b) => b -> Relation a b -> Relation a b
deleteR y (Rel m1 m2) = 
  let (mxs, m2') = updateLookupWithKey (\_ _ -> Nothing) y m2 in
  let xs = fromMaybe [] mxs in
  let m1' = L.foldl' (\m k -> update (remove y) k m) m1 xs in
  Rel m1' m2'

deleteL :: (Ord a, Ord b) => a -> Relation a b -> Relation a b
deleteL x (Rel m1 m2) = 
  let (mys, m1') = updateLookupWithKey (\_ _ -> Nothing) x m1 in
  let ys = fromMaybe [] mys in
  let m2' = L.foldl' (\m k -> update (remove x) k m) m2 ys in
  Rel m1' m2'
