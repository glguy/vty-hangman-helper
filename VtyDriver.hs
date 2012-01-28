module Main (main) where

import Control.Monad (when)
import Data.Char (toUpper)
import Graphics.Vty

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
  h <- markHistory
  m               <- getModel
  ev              <- nextKey (draw Nothing Nothing)
  let validChoices = map fst (currentChoices m)
      genMask      = generateMaskPrefix (currentMask m)
  case ev of
    KASCII c | c `elem` validChoices -> setHistory h >> enterMaskMode c genMask
    KEsc                             -> return ()
    KBS                              -> popHistory
    _                                -> enterLetterMode

enterMaskMode :: Char -> Mask -> Game ()
enterMaskMode c xs = do
  h <- markHistory
  m <- getModel
  ev <- nextKey (draw (Just c) (Just xs))
  let prevMask   = currentMask m
  let growMask k = case extendMask prevMask k xs of
                     Nothing  -> enterMaskMode c xs
                     Just xs' -> setHistory h >> enterMaskMode c xs'
  case ev of
    KASCII k | k == c -> growMask (Just k)
    KASCII ' '        -> growMask Nothing
    KASCII '.'        -> growMask Nothing
    KEnter            -> setHistory h >> finishMask prevMask c xs
    KBS               -> popHistory
    KEsc              -> return ()
    _                 -> enterMaskMode c xs

finishMask :: Mask -> Char -> Mask -> Game ()
finishMask prev c m =
  case extendMask prev Nothing m of
    Nothing -> confirmMask c m
    Just m' -> finishMask prev c m'

confirmMask :: Char -> Mask -> Game ()
confirmMask c mask = do
  m <- getModel
  let g = applyGuess c mask m
  when (currentMask m == mask) incMissCount
  setModel g
  enterLetterMode 

nextKey :: (DisplayRegion -> GameState -> Picture) -> Game Key
nextKey pic = do
  updateG pic
  ev <- nextEventG
  case ev of
    EvKey (KASCII k) [] -> return (KASCII (toUpper k))
    EvKey k []          -> return k
    _                   -> nextKey pic
