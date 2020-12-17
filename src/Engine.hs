{-# LANGUAGE RecordWildCards #-}
module Engine ( createGame
              ) where

import Data.Monoid (First, getFirst)
import Data.Map.Strict (empty)

import Engine.Base
import Types

-- |Check multitude of errors. Returns Nothing if all is fine. Using
-- First monoid (stops and collect only first error).
createGame :: Rules -> [Boat] -> Either LayoutFailure Game
createGame Rules{..} boats = maybe (Right game) Left (getFirst validate)
  where targets = map renderBoat boats
        checkBoundaryMsg target = check OutOfBounds $ checkBoundary board target
        validate = mconcat (map checkBoundaryMsg targets) <>
                   check Overlapping (checkOverlap targets) <>
                   check TooClose (checkClearance keepout targets) <>
                   check CountMismatch (checkShipCount shipset boats)
        game = Game board targets empty

-- |Helper function for First monoid
check :: a -> Bool -> First a
check _ True  = mempty
check a False = pure a
