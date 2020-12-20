{-# LANGUAGE RecordWildCards #-}
module Engine ( createGame
              , strike
              , StrikeMonad
              ) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Map.Strict (empty, insert, notMember)

import Engine.Base
import Types

type LayoutMonad = Except LayoutFailure
type GameMonad e o = ExceptT e (StateT Game o)
type StrikeMonad o = GameMonad StrikeFail o

-- |Creates a new game while checking layout rules. Returns a Game
-- normally but if boat layout is incorrect, an exception is thrown
-- about the most severe error. There might be other errors, too.
createGame :: Rules -> [Boat] -> LayoutMonad Game
createGame Rules{..} boats = do
  mapM (check OutOfBounds . checkBoundary board) targets
  check Overlapping $ checkOverlap targets
  check TooClose $ checkClearance keepout targets
  check CountMismatch $ checkShipCount shipset boats
  pure $ Game board targets empty
  where targets = map renderBoat boats

-- |Strikes given coordinate
strike :: (Monad m) => Coordinate -> StrikeMonad m Outcome
strike coord = do
  Game{..} <- get
  check InvalidCoordinate $ checkCoordBounds gBoard coord
  check AlreadyHit $ coord `notMember` history
  -- Trying to hit one by one, initial state is not hit (Miss) of course.
  let (targetsAfter, outcome) = runState (mapM (strikeM coord) targets) Miss
      newHistory              = insert coord outcome history
  put $ Game gBoard targetsAfter history
  pure outcome

-- |Stateful strike, collects hit/sink if any.
strikeM :: Coordinate -> Target -> State Outcome Target
strikeM coord target = do
  let StrikeTargetResult{..} = strikeTarget coord target
  when (outcome /= Miss) $ put outcome
  pure boatAfter

-- |Helper function for validation.
check :: MonadError e f => e -> Bool -> f ()
check _ True  = pure ()
check e False = throwError e
