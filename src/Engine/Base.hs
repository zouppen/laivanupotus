{-# LANGUAGE RecordWildCards #-}
module Engine.Base where

import Data.List (sort)
import Data.Set (Set, (\\), fromList, toList)
import qualified Data.Set as S

import Types
import Engine.Internal

data StrikeTargetResult = StrikeTargetResult { outcome   :: Outcome
                                             , boatAfter :: Target
                                             } deriving (Show, Eq)

-- |Render a boat from boat definition
renderBoat :: KeepoutZone -> Boat -> Target
renderBoat keepout Boat{..} = renderBoatRaw keepout $ fromList $ case boatOrientation of
  Horizontal -> [ Coordinate (x , boatY) | x <- take boatLength [boatX..]]
  Vertical   -> [ Coordinate (boatX , y) | y <- take boatLength [boatY..]]

renderBoatRaw :: KeepoutZone -> Set Coordinate -> Target
renderBoatRaw (KeepoutZone keepout) coordinates = Target{..}
  where clearance = S.unions [nudge n coordinates | n <- keepout] \\ coordinates

-- |Try to hit the boat
strikeTarget :: Coordinate -> Target -> StrikeTargetResult
strikeTarget x Target{..} = StrikeTargetResult{..}
  where after = S.delete x coordinates
        outcome = if S.member x coordinates
                  then if S.null after
                       then Sink
                       else Hit
                  else Miss
        boatAfter = Target{coordinates=after,..}

-- |Nudge coordinates by given constant
nudge :: (Int, Int) -> Set Coordinate -> Set Coordinate
nudge (x0,y0) = S.mapMonotonic $ \(Coordinate (x,y)) -> Coordinate (x0+x, y0+y)

-- |Check that target fits on board
checkBoundary :: Board -> Target -> Bool
checkBoundary board Target{..} = S.foldr' (\x acc -> acc && checkCoordBounds board x) True coordinates

-- |Check if a single coordinate is on board
checkCoordBounds :: Board -> Coordinate -> Bool
checkCoordBounds Board{..} (Coordinate (x,y)) = x >= minX && x <= maxX && y >= minY && y <= maxY

-- |Check if the ship count matches game specs
checkShipCount :: Shipset -> [Boat] -> Bool
checkShipCount (Shipset ss) boats = ss == (sort $ map boatLength boats)

-- |Check that the boats are not on each other
checkOverlap :: [Target] -> Bool
checkOverlap targets = individualSize == unionSize
  where individualSize = sum (map (S.size . coordinates) targets)
        unionSize = S.size $ S.unions $ map coordinates targets

-- |Check that clearance areas are not touching any boats
checkClearance :: [Target] -> Bool
checkClearance targets = S.null $ targetSet `S.intersection` clearanceSet
  where targetSet = S.unions $ map coordinates targets
        clearanceSet = S.unions $ map clearance targets

-- |False if the ship is sunk, true otherwise (may have hits, though)
isAfloat :: Target -> Bool
isAfloat Target{..} = not $ null coordinates
