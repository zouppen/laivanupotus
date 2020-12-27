{-# LANGUAGE RecordWildCards #-}
module Engine ( createGame
              , shipsLeft
              , strike
              , StrikeMonad
              , LayoutMonad
              , run
              ) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Map.Strict (Map, empty, insert, notMember, union)
import Data.Functor.Identity (Identity)

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
  check TooClose $ checkClearance targets
  check CountMismatch $ checkShipCount shipset boats
  pure $ Game board targets empty
  where targets = map (renderBoat keepout) boats

-- |Strikes given coordinate
strike :: Monad m => Coordinate -> StrikeMonad m Outcome
strike coord = do
  Game{..} <- get
  check InvalidCoordinate $ checkCoordBounds gBoard coord
  check AlreadyHit $ coord `notMember` history
  -- Trying to hit one by one, initial state is not hit (Miss) of course.
  let (targetsAfter, (outcome,exposed)) = runState (mapM (strikeOne coord) targets) (Miss, empty)
      newHistory                        = exposed `union` insert coord outcome history
  put $ Game gBoard targetsAfter newHistory
  pure outcome

-- |Stateful strike, collects hit/sink if any.
strikeOne :: Coordinate -> Target -> State (Outcome, Map Coordinate Outcome) Target
strikeOne coord target = do
  let StrikeTargetResult{..} = strikeTarget coord target
  when (outcome /= Miss) $ put (outcome, exposed)
  pure boatAfter

shipsLeft :: Monad m => StrikeMonad m Int
shipsLeft = do
  Game{..} <- get
  pure $ length $ filter isAfloat targets

-- |Helper function for validation.
check :: MonadError e f => e -> Bool -> f ()
check _ True  = pure ()
check e False = throwError e

-- |Run the game monad and return possible error on the Left and new
-- state in a tuple.
run :: Game -> GameMonad e Identity a -> (Either e a, Game)
run game = flip runState game . runExceptT
