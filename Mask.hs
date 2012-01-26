module Mask where

import Data.Maybe
import Data.List

newtype Mask = Mask [Maybe Char]
  deriving (Eq)

instance Show Mask where
  show (Mask xs) = map (fromMaybe '.') xs

maskElem :: Char -> Mask -> Bool
maskElem c (Mask xs) = any (maybe False (c==)) xs

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

isVowel :: Char -> Bool
isVowel 'A' = True
isVowel 'E' = True
isVowel 'I' = True
isVowel 'O' = True
isVowel 'U' = True
isVowel  _  = False
