module Mask where

import Data.Maybe
import Data.List
import Data.Char

newtype Mask = Mask [Maybe Char]
  deriving (Eq)

instance Show Mask where
  show (Mask xs) = map (fromMaybe '.') xs

maskElem :: Char -> Mask -> Bool
maskElem c (Mask xs) = any (maybe False (c==)) xs

looseMatch :: [Char] -> Mask -> String -> Bool
looseMatch ls (Mask ms) = aux ls ms
  where
  aux _ [] [] = True
  aux letters (Nothing : xs) (y : ys)
    | y `elem` letters = aux (delete y letters) xs ys
  aux letters (Just x  : xs) (y : ys)
    | x == y && y `elem` letters = aux (delete y letters) xs ys
  aux _ _ _ = False

scrubMask :: Mask -> Mask
scrubMask (Mask xs) = Mask (map erase xs)
  where
  v = find isVowel (reverse (catMaybes xs))
             
  erase c | v == c = c
          | otherwise = Nothing

parseMask :: String -> Mask
parseMask xs = Mask (map aux xs)
    where
    aux '.' = Nothing
    aux x   = Just (toUpper x)

isOneStep :: Char -> Mask -> Mask -> Bool
isOneStep c (Mask xs) (Mask ys) = and (zipWith aux xs ys)
  where
  aux Nothing (Just x) = x == c
  aux x       y        = x == y

isSubmaskOf :: Mask -> Mask -> Bool
isSubmaskOf (Mask m1) (Mask m2) = aux m1 m2
  where
  oldLetters = catMaybes m1

  aux []             []             = True
  aux (Nothing : xs) (Nothing : ys) =                           aux xs ys
  aux (Nothing : xs) (Just y  : ys) = y `notElem` oldLetters && aux xs ys
  aux (Just x  : xs) (Just y  : ys) = x == y                 && aux xs ys
  aux _              _              = False

maskLetters :: Mask -> [Char]
maskLetters (Mask xs) = nub (sort (catMaybes xs))

maskLength :: Mask -> Int
maskLength (Mask xs) = length xs

trailingNonvowels :: Mask -> Int
trailingNonvowels (Mask xs) = length (takeWhile (maybe True (not . isVowel)) (reverse xs))

match :: Mask -> [Char] -> String -> Bool
match (Mask xs) banned ys = and (zipWith aux xs ys)
  where
  aux Nothing  y = y `notElem` banned
  aux (Just x) y = x == y

isVowel :: Char -> Bool
isVowel 'A' = True
isVowel 'E' = True
isVowel 'I' = True
isVowel 'O' = True
isVowel 'U' = True
isVowel  _  = False
