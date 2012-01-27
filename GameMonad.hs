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
  , currentHistory   :: Maybe (GameState, Char, Mask)
  }

newGameState :: GameModel -> GameState
newGameState g = GameState
  { currentModel     = g
  , currentMissCount = 0
  , currentHistory   = Nothing
  }

newtype Game a = G (StateT GameState VIO a)
  deriving (Monad, Functor)

updateG :: (GameState -> Picture) -> Game ()
updateG p = G $ do
  g <- get
  lift (updateV (p g))

nextKeyG :: Game Key
nextKeyG = G (lift nextKey)

getModel :: Game GameModel
getModel = G (gets currentModel)

setModel :: GameModel -> Game ()
setModel m = G (modify (\g -> g { currentModel = m}))

incMissCount :: Game ()
incMissCount = G (modify (\g -> g { currentMissCount = currentMissCount g + 1 }))

runGame :: GameState -> Game a -> IO a
runGame g (G m) = runV (fmap fst (runStateT m g))

pushHistory :: Char -> Mask -> Game ()
pushHistory c xs = G (modify (\g -> g { currentHistory = Just (g, c, xs)}))

popHistory :: Game (Maybe (Char, Mask))
popHistory = G $ do
  h <- gets currentHistory
  for h $ \(g, c, xs) -> do
    put g
    return (c, xs)
