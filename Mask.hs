module Mask where

import Data.Char     (toUpper)
import Data.Maybe    (catMaybes, isJust)
import Data.List

newtype Mask = Mask [Maybe Char]
  deriving (Eq, Show)

parseMask :: String -> Maybe Mask
parseMask = fmap Mask . mapM (aux . toUpper)
  where
  aux '.'                   = return Nothing
  aux c | c `elem` alphabet = return (Just c)
  aux _                     = Nothing

maskElem :: Char -> Mask -> Bool
maskElem c (Mask xs) = any (Just c ==) xs

scrubMask :: Mask -> Mask
scrubMask (Mask xs) = Mask (map erase xs)
  where
  v = find isVowel (reverse (catMaybes xs))
             
  erase c | v == c = c
          | otherwise = Nothing

maskLetters :: Mask -> [Char]
maskLetters (Mask xs) = nub (sort (catMaybes xs))

trailingConsonants :: Mask -> Int
trailingConsonants (Mask xs) =
  length (takeWhile (maybe True (not . isVowel)) (reverse xs))

match :: Mask -> [Char] -> String -> Bool
match (Mask xs0) banned ys0 = aux xs0 ys0
  where
  aux (Nothing : xs) (y : ys) = y `notElem` banned && aux xs ys
  aux (Just x  : xs) (y : ys) = x == y             && aux xs ys
  aux []             []       = True
  aux _              _        = False

alphabet :: String
alphabet = ['A'..'Z']

vowels :: String
vowels = "AEIOU"

isVowel :: Char -> Bool
isVowel x = x `elem` vowels

-- * Mask editing functions

extendMask :: Mask -> Maybe Char -> Mask -> Maybe Mask
extendMask (Mask template) c (Mask input) = fmap Mask (aux template input)
  where
  aux (_ : xs) (y : ys) = fmap (y :) (aux xs ys)
  aux []       _        = Nothing
  aux (_ : xs) _        = Just (c : takeWhile isJust xs)

generateMaskPrefix :: Mask -> Mask
generateMaskPrefix (Mask template) = Mask (takeWhile isJust template)
