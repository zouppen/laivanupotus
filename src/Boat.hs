{-# LANGUAGE RecordWildCards #-}
module Boat where

import Data.Monoid (getFirst)
import Data.List (sort)
import Data.Set (Set, (\\), fromList, toList)
import qualified Data.Set as S

import Types
import Boat.Internal

-- |Render a boat from boat definition
renderBoat :: Boat -> Target
renderBoat Boat{..} = Target $ fromList $ case boatOrientation of
  Horizontal -> [ Coordinate (x , boatY) | x <- take boatLength [boatX..]]
  Vertical   -> [ Coordinate (boatX , y) | y <- take boatLength [boatY..]]

-- |Try to hit the boat
strike :: Coordinate -> Target -> StrikeResult
strike x (Target before) = StrikeResult outcome $ Target after
  where after = S.delete x before
        outcome = if S.member x before
                  then if S.null after
                       then Sink
                       else Hit
                  else Miss

-- |Nudge coordinates by given constant
nudge :: (Int, Int) -> Set Coordinate -> Set Coordinate
nudge (x0,y0) = S.mapMonotonic $ \(Coordinate (x,y)) -> Coordinate (x0+x, y0+y)

-- |Clearance coordinates around the ship
clearance :: KeepoutZone -> Target -> Clearance
clearance (KeepoutZone keepout) (Target s) = Clearance $ S.unions [nudge n s | n <- keepout] \\ s

-- |Check that target fits on board
checkBoundary :: Board -> Target -> Bool
checkBoundary Board{..} (Target s) = S.foldr' (\x acc -> acc && bounds x) True s
  where bounds (Coordinate (x,y)) = x >= minX && x <= maxX && y >= minY && y <= maxY

-- |Check if the ship count matches game specs
checkShipCount :: Shipset -> [Boat] -> Bool
checkShipCount (Shipset ss) boats = ss == (sort $ map boatLength boats)

-- |Check that the boats are not on each other
checkOverlap :: [Target] -> Bool
checkOverlap targets = individualSize == unionSize
  where individualSize = sum (map (S.size . unwrapT) targets)
        unionSize = S.size $ S.unions $ map unwrapT targets

-- |Check that clearance areas are not touching any boats
checkClearance :: KeepoutZone -> [Target] -> Bool
checkClearance keepout targets = S.null $ targetSet `S.intersection` clearanceSet
  where targetSet = S.unions $ map unwrapT targets
        clearanceSet = S.unions $ map (unwrapC . clearance keepout) targets

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
