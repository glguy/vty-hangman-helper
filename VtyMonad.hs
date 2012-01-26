{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module VtyMonad where

import Data.Char
import Graphics.Vty
import Control.Exception (bracket)
import Control.Monad.Reader

newtype VIO a = V (ReaderT Vty IO a)
  deriving (Monad, Functor)

updateV :: Picture -> VIO ()
updateV x = V (ask >>= \vty -> lift (update vty x))

next_eventV :: VIO Event
next_eventV = V (ask >>= \vty -> lift (next_event vty))

next_key :: VIO Key
next_key = do
  ev <- next_eventV
  case ev of
    EvKey (KASCII a) [] -> return (KASCII (toUpper a))
    EvKey k []          -> return k
    _                   -> next_key

runV :: VIO a -> IO a
runV (V f) = bracket mkVty shutdown (runReaderT f)
