{-# LANGUAGE RecordWildCards #-}
module Engine ( checkRules
              ) where

import Data.Monoid (getFirst)

import Engine.Base
import Types

-- |Check multitude of errors. Returns Nothing if all is fine. Using
-- First monoid (stops and collect only first error).
checkRules :: Rules -> [Boat] -> Maybe LayoutFailure
checkRules Rules{..} boats = getFirst $
  mconcat (map checkBoundaryMsg targets) <>
  check Overlapping (checkOverlap targets) <>
  check TooClose (checkClearance keepout targets) <>
  check CountMismatch (checkShipCount shipset boats)
  where targets = map renderBoat boats
        checkBoundaryMsg target = check OutOfBounds $ checkBoundary board target
        check _ True  = mempty
        check a False = pure a
