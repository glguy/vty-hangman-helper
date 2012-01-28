module GameModel
  (GameModel(..), newGameModel,
   alphabet, currentChoices, applyGuess)
  where

import Data.List
import Data.Ord

import Mask

data GameModel = GameModel
  { lettersTried :: [Char]
  , currentMask  :: Mask
  , currentWords :: [String]
  }
  deriving (Show)

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

rankLetter :: Char -> [String] -> Int
rankLetter x = count (elem x)

rankLetters :: [Char] -> [String] -> [(Char, Int)]
rankLetters xs ws = sortBy (flip (comparing snd))
                     [(x, i) | x <- xs, let i = rankLetter x ws, i > 0]

currentChoices :: GameModel -> [(Char, Int)]
currentChoices g = rankLetters (alphabet \\ lettersTried g) (currentWords g)

newGameModel :: Mask -> [String] -> GameModel
newGameModel mask ws = GameModel { lettersTried = lettersTried'
                                 , currentMask  = mask
                                 , currentWords = initialWords ws
                                 }
  where
  initialWords  = filter checkMask . filter checkVowels

  vowelCount    = trailingConsonants mask
  checkVowels   = all (not . isVowel) . take vowelCount . reverse

  lettersTried' = maskLetters mask
  checkMask     = match mask lettersTried'


applyGuess :: Char -> Mask -> GameModel -> GameModel
applyGuess c newmask g = GameModel
  { lettersTried = lettersTried'
  , currentMask  = newmask
  , currentWords = filter (match newmask lettersTried') (currentWords g)
  }
  where
  lettersTried' = c : lettersTried g
