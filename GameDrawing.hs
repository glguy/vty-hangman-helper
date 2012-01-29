module GameDrawing (draw) where

import Graphics.Vty
import Data.List     (intersperse)
import Data.Maybe    (fromMaybe, isNothing)

import GameMonad (GameState(..))
import GameModel
import Mask

-- * Render functions

gray :: Color
gray = Color240 204

draw :: Maybe Char -> Maybe Mask -> DisplayRegion -> GameState -> Picture
draw c xs region s =
  Picture { pic_cursor     = cursor
          , pic_background = Background ' ' current_attr
          , pic_image      = tophalf <-> wordboxes
          }
  where
  tophalf = infobox
        <-> drawChoice c
        <-> drawMaskInput (currentMask g) xs

  maxStrikes = max 0 (12 - maskLen)

  infobox =
         drawLetterBox g region
     <-> string def_attr "Words remaining: "
     <|> string (fore red) (show (length (currentWords g)))
     <-> string def_attr "        Strikes: "
     <|> drawStrikes maxStrikes (currentMissCount s)
     <-> string def_attr "        Letters: "
     <|> usedLettersText g
     <-> char def_attr ' '

  n = case xs of
        Nothing       -> 0
        Just (Mask k) -> length k

  cursor | isNothing c = Cursor 10 (image_height infobox)
         | otherwise   = Cursor (fromIntegral n + 10)
                                (image_height infobox + 1)

  g = currentModel s

  maskLen = let Mask k = currentMask g in length k

  wordlines = map (horiz_cat . intersperse (string def_attr "  "))
            $ chunks numWordWidth
            $ map (highlightPossibility (currentMask g))
            $ currentWords g

  numWordLines = fromIntegral (region_height region)
               - fromIntegral (image_height tophalf)

  numWordWidth = (fromIntegral (region_width region) + 2)
           `div` (maskLen + 2)

  wordboxes = vert_cat $ char def_attr ' ' : take numWordLines wordlines

drawStrikes :: Int -> Int -> Image
drawStrikes maxStrikes strikes =
      string (fore red)  (replicate (min strikes maxStrikes) 'X')
  <|> string (fore gray) (replicate (maxStrikes - strikes)   'X')
  <|> overstrikes
  where
  overstrikes
    | strikes <= maxStrikes = empty_image
    | otherwise = string (fore red) (" +" ++ show (strikes - maxStrikes))

highlightPossibility :: Mask -> String -> Image
highlightPossibility (Mask template) str =
  horiz_cat $ zipWith aux template str
  where
  aux Nothing c = char def_attr    c
  aux _       c = char (fore gray) c

usedLettersText :: GameModel -> Image
usedLettersText g = horiz_cat $ map pick alphabet
  where
  pick x
    | x `notElem` lettersTried g = char (fore yellow) x
    | x `maskElem` currentMask g = char (with_style (fore green) bold) x
    | otherwise                  = char (with_style (fore red)   bold) x

drawMaskInput :: Mask -> Maybe Mask -> Image
drawMaskInput previous Nothing = string def_attr ("    Mask: " ++ maskString previous)
drawMaskInput (Mask previous) (Just (Mask str))
    = string def_attr "    Mask: "
  <|> horiz_cat
        (zipWith drawMaskInputChar previous (map Just str ++ repeat Nothing))

drawMaskInputChar :: Maybe Char -> Maybe (Maybe Char) -> Image
drawMaskInputChar x Nothing  = char def_attr (fromMaybe '.' x)
drawMaskInputChar x (Just (Just y)) | x == Just y = char def_attr y
drawMaskInputChar _ (Just y) = char (fore red) (fromMaybe '.' y)

drawChoice :: Maybe Char -> Image
drawChoice Nothing  = string def_attr "  Choice:"
drawChoice (Just c) = string def_attr "  Choice: "
                  <|> char (fore red) c

drawLetterBox :: GameModel -> DisplayRegion -> Image
drawLetterBox g region
  | null xs   = boxImage (string def_attr "No letters available")
  | otherwise = boxImage (generate xs)
  where
  numDivisions = fromIntegral (region_width region) `div` 8
  xs = currentChoices g
  generate = horiz_cat
           . intersperse (string def_attr "  ")
           . map vert_cat
           . divisions numDivisions
           . map drawLetter

drawLetter :: (Char,Int) -> Image
drawLetter (c,i) = string def_attr (c : ": ")
               <|> string (fore red) (padded 3 (show i))

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

maskString :: Mask -> String
maskString (Mask xs) = map (fromMaybe '.') xs

-- ** Utilities

fore :: Color -> Attr
fore = with_fore_color def_attr

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
