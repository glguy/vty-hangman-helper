{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VtyMonad where

import Control.Exception (bracket)
import MonadLib (ReaderT, ask, lift, runReaderT)
import Data.Char (toUpper)
import Graphics.Vty
  (Picture, DisplayRegion(..), Event(..), Key(..), Vty(..),
   display_bounds, mkVty)

newtype VIO a = V (ReaderT Vty IO a)
  deriving (Monad, Functor)

runV :: VIO a -> IO a
runV (V f) = bracket mkVty shutdown (\vty -> runReaderT vty f)

updateV :: Picture -> VIO ()
updateV x = V $ do
  vty <- ask
  lift (update vty x)

displayRegion :: VIO DisplayRegion
displayRegion = V $ do
  vty <- ask
  lift (display_bounds (terminal vty))

nextEvent :: VIO Event
nextEvent = V $ do
  vty <- ask
  lift (next_event vty)

nextKey :: VIO Key
nextKey = do
  ev  <- nextEvent
  case ev of
    EvKey (KASCII a) [] -> return (KASCII (toUpper a))
    EvKey k []          -> return k
    _                   -> return KMenu
