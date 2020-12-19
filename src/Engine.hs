{-# LANGUAGE RecordWildCards #-}
module Engine ( createGame
              ) where

import Control.Monad.Except
import Data.Map.Strict (empty)

import Engine.Base
import Types

type LayoutMonad = Except LayoutFailure

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

-- |Helper function for validation.
check :: MonadError e f => e -> Bool -> f ()
check _ True  = pure ()
check e False = throwError e
