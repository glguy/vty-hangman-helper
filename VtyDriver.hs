module Main (main) where

import Control.Monad (when)
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
  mask                 <- case parseMask startingMask of
                            Nothing -> fail "Unable to parse mask"
                            Just m  -> return m
  let s0                = constructFirstGameState opts mask ws
  runGame s0 enterLetterMode

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
  updateG (draw Nothing Nothing)
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

enterMaskMode :: Char -> Mask -> Game ()
enterMaskMode c xs = do
  m <- getModel
  let prevMask   = currentMask m
      growMask k = extendMask prevMask k xs
  updateG (draw (Just c) (Just xs))
  ev <- nextKeyG
  case ev of
    KASCII k | k == c -> enterMaskMode c (growMask (Just k))
    KASCII ' '        -> enterMaskMode c (growMask Nothing)
    KASCII '.'        -> enterMaskMode c (growMask Nothing)
    KBS               -> case retractMask prevMask xs of
                           Nothing -> enterLetterMode
                           Just ys -> enterMaskMode c ys
    KEnter            -> confirmMask c (completeMask prevMask xs)
    KEsc              -> return ()
    _                 -> enterMaskMode c xs

confirmMask :: Char -> Mask -> Game ()
confirmMask c mask = do
  m <- getModel
  let g = applyGuess c mask m
  pushHistory c mask
  when (currentMask m == mask) incMissCount
  setModel g
  enterLetterMode 
