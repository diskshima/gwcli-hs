module StringUtils
  (
    orBlank
  , replace
  ) where

import           Data.Maybe (fromMaybe)

orBlank :: Maybe String -> String
orBlank = fromMaybe ""

-- Token from
--   https://stackoverflow.com/a/14880881
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle replacement haystack
  = case begins haystack needle of
      Just remains -> replacement ++ remains
      Nothing      -> case haystack of
                        []     -> []
                        x : xs -> x : replace needle replacement xs

begins :: Eq a => [a] -> [a] -> Maybe [a]
begins haystack []       = Just haystack
begins (x : xs) (y : ys) | x == y = begins xs ys
begins _        _        = Nothing
