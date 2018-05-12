module ListUtils
  (
    nthOrDefault
  ) where

nthOrDefault :: [a] -> a -> Integer -> a
nthOrDefault arr defValue index = nthOrDefaultInner arr defValue index 0

nthOrDefaultInner :: [a] -> a -> Integer -> Integer -> a
nthOrDefaultInner [] defValue _ _ = defValue
nthOrDefaultInner (x: xs) defValue index curIndex =
  if index == curIndex
   then x
   else nthOrDefaultInner xs defValue index (curIndex + 1)
