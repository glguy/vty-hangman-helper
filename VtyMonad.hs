module VtyMonad where

import Data.Char
import Graphics.Vty
import Control.Exception (bracket)

newtype VIO a = V (Vty -> IO a)

instance Functor VIO where
  fmap f (V m) = V (fmap f . m)

instance Monad VIO where
  return x   = V (\_ -> return x)
  V m >>= f  = V (\vty -> m vty >>= \ x -> let V m' = f x in m' vty)
  fail err   = V (\_   -> fail err)
  V m >> V n = V (\vty -> m vty >> n vty)

lift :: IO a -> VIO a
lift m = V (\_ -> m)

updateV :: Picture -> VIO ()
updateV x = V (\vty -> update vty x)

next_eventV :: VIO Event
next_eventV = V next_event

next_key :: VIO Key
next_key = do
  ev <- next_eventV
  case ev of
    EvKey (KASCII a) [] -> return (KASCII (toUpper a))
    EvKey k []          -> return k
    _                   -> next_key

runV :: VIO a -> IO a
runV (V f) = bracket mkVty shutdown f
