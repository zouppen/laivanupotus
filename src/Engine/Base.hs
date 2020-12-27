{-# LANGUAGE RecordWildCards #-}
module Engine.Base where

import Data.List (sort)
import Data.Set (Set, (\\), fromList, toList)
import qualified Data.Set as S
import Data.Map.Strict (Map, fromSet, empty, singleton, insert)
import qualified Data.Map.Strict as M

import Types
import Engine.Internal

data StrikeTargetResult = StrikeTargetResult { exposed   :: Map Coordinate Outcome
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
        exposed = if S.member x coordinates
                  then if S.null after
                       then insert x Sink $ fromSet (const Close) clearance
                       else singleton x Hit
                  else empty -- Miss
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

-- |Collect game statistics by digging game structure.
renderStats :: Game -> Stats
renderStats Game{..} = Stats{..}
  where boatsSunk  = length $ filter (S.null . coordinates) targets
        boatsTotal = length targets
        known      = M.size history
        hits       = M.foldr (summer hit) 0 history
        turns      = M.foldr (summer turner) 0 history
        -- Count only when hit something
        hit Hit    = 1
        hit Sink   = 1
        hit _      = 0
        -- Turn is everything except exposed cells
        turner Close = 0
        turner _     = 1

summer :: Num b => (a -> b) -> a -> b -> b
summer f val acc = acc + f val
