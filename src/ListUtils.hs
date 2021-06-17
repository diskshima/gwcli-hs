module ListUtils
  (
    dropLast
  , firstMatching
  , formatEachAndJoin
  , nthOrDefault
  , nthOrNothing
  , replace
  , split
  , takeLast
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

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new list = intercalate new (split old list)

split :: Eq a => [a] -> [a] -> [[a]]
split _    [] = [[]]
split item xs = split' item xs []

split' :: Eq a => [a] -> [a] -> [a] -> [[a]]
split' _    []     acc = [acc]
split' item (x:xs) acc =
  if item == takeLast (length item) nextAcc
     then dropLast (length item - 1) acc : split' item xs []
     else split' item xs nextAcc
  where nextAcc = acc ++ [x]

dropLast :: Int -> [a] -> [a]
dropLast len list = take (length list - len) list

takeLast :: Int -> [a] -> [a]
takeLast len list = drop (length list - len) list
