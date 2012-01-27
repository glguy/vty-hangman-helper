module GameDrawing (draw) where

import Graphics.Vty.Picture
import Data.List     (intercalate, intersperse)
import Data.Maybe    (fromMaybe, isJust, isNothing)

import GameMonad (GameState(..))
import GameModel
import Mask

-- * Render functions

draw :: Maybe Char -> Maybe Mask -> GameState -> Picture
draw c xs s =
  Picture { pic_cursor     = cursor
          , pic_background = Background ' ' current_attr
          , pic_image      = image
          }
  where
  image = topbox
      <-> drawChoice c
      <-> drawMaskInput (currentMask g) xs
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

  n = case xs of
        Nothing -> 0
        Just (Mask k) -> length k
  cursor | isNothing c = Cursor 10 (image_height topbox)
         | otherwise   = Cursor (fromIntegral n + 10)
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

drawMaskInput :: Mask -> Maybe Mask -> Image
drawMaskInput previous Nothing = string def_attr ("    Mask: " ++ maskString previous)
drawMaskInput (Mask previous) (Just (Mask str))
    = string def_attr "    Mask: "
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
drawLetterBox g
  | null xs   = boxImage (string def_attr "No letters available")
  | otherwise = boxImage (generate xs)
  where
  xs = currentChoices g
  generate = horiz_cat
           . intersperse (string def_attr "  ")
           . map vert_cat
           . divisions 8
           . map drawLetter

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

maskString :: Mask -> String
maskString (Mask xs) = map (fromMaybe '.') xs

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
