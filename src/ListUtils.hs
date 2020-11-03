module ListUtils
  (
    firstMatching
  , formatEachAndJoin
  , nthOrDefault
  , nthOrNothing
  ) where

import           Data.List  (find, intercalate)
import           Data.Maybe (fromMaybe)

nthOrDefault :: [a] -> a -> Integer -> a
nthOrDefault arr defValue index = fromMaybe defValue (nthOrNothing arr index)

nthOrNothing :: [a] -> Integer -> Maybe a
nthOrNothing arr index = nthOrNothingInner arr index 0

nthOrNothingInner :: [a] -> Integer -> Integer -> Maybe a
nthOrNothingInner [] _ _ = Nothing
nthOrNothingInner (x: xs) index curIndex =
  if index == curIndex
   then Just x
   else nthOrNothingInner xs index (curIndex + 1)

formatEachAndJoin :: [a] -> (a -> String) -> String
formatEachAndJoin list formatter = intercalate "\n" $ fmap formatter list

firstMatching :: Eq a => [a] -> [a] -> Maybe a
firstMatching items = find matches
  where matches candidate = candidate `elem` items
