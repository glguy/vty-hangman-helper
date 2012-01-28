{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMonad where

import Control.Monad.State
import Data.Traversable
import Graphics.Vty

import GameModel
import Mask
import VtyMonad

data GameState = GameState
  { currentModel     :: GameModel
  , currentMissCount :: Int
  , currentHistory   :: Maybe (GameState, Maybe (Char, Mask))
  }

newGameState :: GameModel -> GameState
newGameState g = GameState
  { currentModel     = g
  , currentMissCount = 0
  , currentHistory   = Nothing
  }

newtype Game a = G (StateT GameState VIO a)
  deriving (Monad, Functor)

updateG :: (DisplayRegion -> GameState -> Picture) -> Game ()
updateG p = G $ do
  h <- lift displayRegion
  g <- get
  lift (updateV (p h g))

nextEventG :: Game Event
nextEventG = G (lift nextEvent)

getModel :: Game GameModel
getModel = G (gets currentModel)

setModel :: GameModel -> Game ()
setModel m = G (modify (\g -> g { currentModel = m}))

incMissCount :: Game ()
incMissCount = G (modify (\g -> g { currentMissCount = currentMissCount g + 1 }))

runGame :: GameState -> Game a -> IO a
runGame g (G m) = runV (fmap fst (runStateT m g))

pushHistory :: Maybe (Char, Mask) -> Game ()
pushHistory h = G (modify (\g -> g { currentHistory = Just (g, h)}))

popHistory :: Game (Maybe (Char, Mask))
popHistory = G $ do
  h <- gets currentHistory
  case h of
    Nothing      -> return Nothing
    Just (g, mb) -> do
      put g
      return mb
