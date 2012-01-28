{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMonad where

import Graphics.Vty
import MonadLib

import GameModel
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

liftV :: VIO a -> Game a
liftV = G . lift . lift

nextEventG :: Game Event
nextEventG = liftV nextEvent

updateG :: (DisplayRegion -> GameState -> Picture) -> Game ()
updateG p = do
  g <- G get
  h <- liftV displayRegion
  liftV (updateV (p h g))

getModel :: Game GameModel
getModel = G (liftM currentModel get)

setModel :: GameModel -> Game ()
setModel m = G (modify (\g -> g { currentModel = m}))

incMissCount :: Game ()
incMissCount = G (modify (\g -> g { currentMissCount = currentMissCount g + 1 }))

runGame :: GameState -> Game () -> IO ()
runGame g m = runV $ runContT return $ fmap fst $ runStateT g $ unG $ do
  mark <- markHistory
  setHistory mark
  m

newtype Undo = U (Label (StateT GameState (ContT () VIO)) ())

markHistory :: Game Undo
markHistory = do
  ((), m) <- G (labelCC ())
  return (U m)

setHistory :: Undo -> Game ()
setHistory u = G (modify (\g -> g { currentHistory = u }))

popHistory :: Game a
popHistory = G $ do
  U h <- liftM currentHistory get
  jump () h

-- * MonadLib helper functions

modify :: StateM m s => (s -> s) -> m ()
modify f = set . f =<< get
