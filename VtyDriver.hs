{-# LANGUAGE PatternGuards #-}
module Main (main) where

import Data.List
import Data.Maybe
import Graphics.Vty

import Mask
import GameModel
import VtyMonad
import Options

defaultWordList :: FilePath
defaultWordList = "wordlist1.txt"

wordListIO :: IO [String]
wordListIO = fmap lines (readFile defaultWordList)

data GameState = GameState
  { currentModel     :: GameModel
  , currentMissCount :: Int
  , currentHistory   :: Maybe (GameState, Char, [Maybe Char])
  }

newGameState :: GameModel -> GameState
newGameState g = GameState
  { currentModel     = g
  , currentMissCount = 0
  , currentHistory   = Nothing
  }

main :: IO ()
main = do
  (opts, startingMask)     <- parseOptions
  ws                       <- wordListIO
  let mask0                = parseMask startingMask
      mask1
        | scrubOption opts = scrubMask mask0
        | otherwise        = mask0
      g                    = newGameModel mask1 ws
      s                    = newGameState g
  runV (enterLetterMode s)

enterLetterMode :: GameState -> VIO ()
enterLetterMode g = do
  let validChoices = map fst (currentChoices (currentModel g))
      genMask      = generateMaskPrefix (currentMask (currentModel g))
  updateV (draw g Nothing [])
  ev <- next_key
  case ev of
    KASCII c | c `elem` validChoices          -> enterMaskMode g c genMask
    KBS | Just (h, c, xs) <- currentHistory g -> enterMaskMode h c xs
    KEsc                                      -> return ()
    _                                         -> enterLetterMode g

enterMaskMode :: GameState -> Char -> [Maybe Char] -> VIO ()
enterMaskMode g c xs = do
  let prevMask   = currentMask (currentModel g)
      growMask k = extendMask prevMask k xs
  updateV (draw g (Just c) xs)
  ev <- next_key
  case ev of
    KASCII k | k == c -> enterMaskMode g c (growMask (Just k))
    KASCII ' '        -> enterMaskMode g c (growMask Nothing)
    KASCII '.'        -> enterMaskMode g c (growMask Nothing)
    KBS               -> case retractMask prevMask xs of
                           Nothing -> enterLetterMode g
                           Just ys -> enterMaskMode g c ys
    KEnter            -> case completeMask prevMask xs of
                           Nothing -> confirmMask   g c xs
                           Just ys -> enterMaskMode g c ys
    KEsc              -> return ()
    _                 -> enterMaskMode g c xs

confirmMask :: GameState -> Char -> [Maybe Char] -> VIO ()
confirmMask s c mask =
  case applyGuess c (Mask mask) (currentModel s) of
    Left err -> fail ("Logic bug in mask generation: " ++ err)
    Right g  -> enterLetterMode GameState
                  { currentModel     = g
                  , currentMissCount = miss
                  , currentHistory   = Just (s, c, mask)
                  }
  where
  miss
    | currentMask (currentModel s) == Mask mask = currentMissCount s + 1
    | otherwise                                 = currentMissCount s

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

-- * Render functions

draw :: GameState -> Maybe Char -> [Maybe Char] -> Picture
draw s c xs =
  Picture { pic_cursor     = cursor
          , pic_background = Background ' ' current_attr
          , pic_image      = image
          }
  where
  image = topbox
      <-> drawChoice c
      <-> drawMaskInput (isJust c) (currentMask g) xs
      <-> wordboxes

  topbox =
         drawLetterBox g
     <-> string def_attr "Words remaining: "
     <|> string (with_fore_color def_attr red) (show (length (currentWords g)))
     <-> string def_attr "        Strikes: "
     <|> string (with_fore_color def_attr red) (show (currentMissCount s))
     <-> string def_attr "        Letters: "
     <|> usedLettersText g
     <-> char def_attr ' '
     <-> string def_attr "Old Mask: "
     <|> string def_attr (show (currentMask g))

  cursor | isNothing c = Cursor 10 (image_height topbox)
         | otherwise   = Cursor (genericLength xs + 10)
                                (image_height topbox + 1)

  g = currentModel s
  wordlines = map (intercalate "  ") (chunks 9 (currentWords g))
  wordboxes
    | longerThan 10 wordlines = empty_image
    | otherwise = vert_cat (char def_attr ' ' : map (string def_attr) wordlines)

usedLettersText :: GameModel -> Image
usedLettersText g = horiz_cat $ map pick alphabet
  where
  pick x
    | x `notElem` lettersTried g = char (with_fore_color def_attr yellow) x
    | x `maskElem` currentMask g = char (with_fore_color (with_style def_attr bold) green) x
    | otherwise                  = char (with_fore_color (with_style def_attr bold) red) x

drawMaskInput :: Bool -> Mask -> [Maybe Char] -> Image
drawMaskInput False _               _ = string def_attr "New Mask:"
drawMaskInput True  (Mask previous) str
    = string def_attr "New Mask: "
  <|> horiz_cat
        (zipWith drawMaskInputChar previous (map Just str ++ repeat Nothing))

drawMaskInputChar :: Maybe Char -> Maybe (Maybe Char) -> Image
drawMaskInputChar x Nothing  = char def_attr (fromMaybe '.' x)
drawMaskInputChar x (Just (Just y)) | x == Just y = char def_attr y
drawMaskInputChar _ (Just y) = char (with_fore_color def_attr red) (fromMaybe '.' y)

drawChoice :: Maybe Char -> Image
drawChoice Nothing  = string def_attr "  Choice:"
drawChoice (Just c) = string def_attr "  Choice: "
                  <|> char (with_fore_color def_attr red) c

drawLetterBox :: GameModel -> Image
drawLetterBox = boxImage
              . horiz_cat
              . intersperse (string def_attr "  ")
              . map vert_cat
              . divisions 8
              . map drawLetter
              . currentChoices

drawLetter :: (Char,Int) -> Image
drawLetter (c,i) = string def_attr (c : ": ")
               <|> string (with_fore_color def_attr red) (padded 3 (show i))

boxImage :: Image -> Image
boxImage img = c '┌' <|> hbar <|> c '┐'
           <-> vbar  <|> img  <|> vbar
           <-> c '└' <|> hbar <|> c '┘'
           
  where
  w = image_width img
  h = image_height img
  c = char def_attr
  hbar = char_fill def_attr '─' w 1
  vbar = char_fill def_attr '│' 1 h

-- ** Utilities

padded :: Int -> String -> String
padded i xs = replicate (i - length xs) ' ' ++ xs

divisions :: Int -> [a] -> [[a]]
divisions _ [] = []
divisions n xs = a : divisions (n-1) b
  where
  (a,b) = splitAt ((len + n - 1) `div` n) xs
  len = length xs
  
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks i xs = a : chunks i b
  where
  (a,b) = splitAt i xs

longerThan :: Int -> [a] -> Bool
longerThan i = not . null . drop i
