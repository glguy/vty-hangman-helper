{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMonad where

import MonadLib
import Data.Traversable
import Graphics.Vty

import GameModel
import Mask
import VtyMonad

data GameState = GameState
  { currentModel     :: GameModel
  , currentMissCount :: Int
  , currentHistory   :: Undo
  }

newGameState :: GameModel -> GameState
newGameState g = GameState
  { currentModel     = g
  , currentMissCount = 0
  , currentHistory   = error "no history"
  }

newtype Game a = G { unG :: StateT GameState (ContT () VIO) a }
  deriving (Monad, Functor)

liftV = lift . lift

updateG :: (DisplayRegion -> GameState -> Picture) -> Game ()
updateG p = G $ do
  h <- liftV displayRegion
  g <- get
  liftV (updateV (p h g))

nextEventG :: Game Event
nextEventG = G (liftV nextEvent)

getModel :: Game GameModel
getModel = G (gets currentModel)

setModel :: GameModel -> Game ()
setModel m = G (modify (\g -> g { currentModel = m}))

incMissCount :: Game ()
incMissCount = G (modify (\g -> g { currentMissCount = currentMissCount g + 1 }))

runGame :: GameState -> Game () -> IO ()
runGame g m = runV $ runContT return $ fmap fst $ runStateT g $ unG $ do
  setHistory =<< markHistory
  m

newtype Undo = U (Label (StateT GameState (ContT () VIO))  ())

modify f = set . f =<< get
gets f = fmap f get

markHistory :: Game Undo
markHistory = do
  ((), m) <- G (labelCC ())
  return (U m)

setHistory :: Undo -> Game ()
setHistory u = G (modify (\g -> g { currentHistory = u }))

popHistory :: Game ()
popHistory = do U h <- G (gets currentHistory)
                G (jump () h)
