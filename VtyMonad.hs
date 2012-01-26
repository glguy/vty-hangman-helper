{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VtyMonad where

import Control.Exception (bracket)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Char (toUpper)
import Graphics.Vty (Picture, Event(..), Key(..), Vty(..), mkVty)

newtype VIO a = V (ReaderT Vty IO a)
  deriving (Monad, Functor)

runV :: VIO a -> IO a
runV (V f) = bracket mkVty shutdown (runReaderT f)

updateV :: Picture -> VIO ()
updateV x = V $ do
  vty <- ask
  lift (update vty x)

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
    _                   -> nextKey
