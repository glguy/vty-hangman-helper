module Main (main) where

import Control.Monad (when)
import Data.Char     (toUpper)
import Data.Maybe    (isJust)
import Graphics.Vty.LLInput (Key(..))

import GameModel
import GameDrawing (draw)
import GameMonad
import Mask
import Options

main :: IO ()
main = do
  (opts, startingMask) <- parseOptions
  ws                   <- wordListIO opts
  mask                 <- parseMask startingMask
  let s0                = constructFirstGameState opts mask ws
  runGame s0 enterLetterMode

parseMask :: String -> IO Mask
parseMask = fmap Mask . mapM (aux . toUpper)
  where
  aux '.'                   = return Nothing
  aux c | c `elem` alphabet = return (Just c)
  aux _                     = fail "Bad initial word"

constructFirstGameState :: Options -> Mask -> [String] -> GameState
constructFirstGameState opts rawMask ws = newGameState model
  where
  model                = newGameModel filteredMask ws
  filteredMask
    | scrubOption opts = scrubMask rawMask
    | otherwise        = rawMask

wordListIO :: Options -> IO [String]
wordListIO o = fmap lines (readFile (wordlistFile o))

enterLetterMode :: Game ()
enterLetterMode = do
  m   <- getModel
  let validChoices = map fst (currentChoices m)
      genMask      = generateMaskPrefix (currentMask m)
  updateG (draw Nothing [])
  ev <- nextKeyG
  case ev of
    KASCII c | c `elem` validChoices -> enterMaskMode c genMask
    KEsc                             -> return ()
    KBS                              -> attemptRollback
    _                                -> enterLetterMode

attemptRollback :: Game ()
attemptRollback = do
  mbh <- popHistory
  case mbh of
    Just (c, xs) -> enterMaskMode c xs
    Nothing      -> enterLetterMode

enterMaskMode :: Char -> [Maybe Char] -> Game ()
enterMaskMode c xs = do
  m <- getModel
  let prevMask   = currentMask m
      growMask k = extendMask prevMask k xs
  updateG (draw (Just c) xs)
  ev <- nextKeyG
  case ev of
    KASCII k | k == c -> enterMaskMode c (growMask (Just k))
    KASCII ' '        -> enterMaskMode c (growMask Nothing)
    KASCII '.'        -> enterMaskMode c (growMask Nothing)
    KBS               -> case retractMask prevMask xs of
                           Nothing -> enterLetterMode
                           Just ys -> enterMaskMode c ys
    KEnter            -> case completeMask prevMask xs of
                           Nothing -> confirmMask   c xs
                           Just ys -> enterMaskMode c ys
    KEsc              -> return ()
    _                 -> enterMaskMode c xs

confirmMask :: Char -> [Maybe Char] -> Game ()
confirmMask c mask = do
  m <- getModel
  let g = applyGuess c (Mask mask) m
  pushHistory c mask
  when (currentMask m == Mask mask) incMissCount
  setModel g
  enterLetterMode 

-- * Mask editing functions

extendMask :: Mask -> Maybe Char -> [Maybe Char] -> [Maybe Char]
extendMask (Mask template) c = aux template
  where
  aux []       _        = []
  aux (_ : ys) (x : xs) = x : aux ys xs
  aux (_ : ys) []       = c : takeWhile isJust ys

generateMaskPrefix :: Mask -> [Maybe Char]
generateMaskPrefix (Mask template) = takeWhile isJust template

retractMask :: Mask -> [Maybe Char] -> Maybe [Maybe Char]
retractMask (Mask template) input
  | all (isJust . fst) step1 = Nothing
  | otherwise                = Just (map snd step1)
  where
  step1 = reverse
        $ drop 1
        $ dropWhile (isJust . fst)
        $ reverse
        $ zip template input

completeMask :: Mask -> [Maybe Char] -> Maybe [Maybe Char]
completeMask (Mask template) = aux template
  where
  aux (_ : xs) (y : ys) = fmap (y :) (aux xs ys)
  aux []       []       = Nothing
  aux xs       _        = Just xs

