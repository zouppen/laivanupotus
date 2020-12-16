{-# LANGUAGE RecordWildCards #-}
module Boat where

import Data.Set (Set, (\\), fromList, toList)
import qualified Data.Set as S

data BoatOrientation = Horizontal | Vertical deriving (Show, Eq)

data Outcome = Miss | Hit | Sink deriving (Show, Eq)

data Boat = Boat { boatX           :: Int
                 , boatY           :: Int
                 , boatLength      :: Int
                 , boatOrientation :: BoatOrientation
                 } deriving (Show, Eq)

data StrikeResult = StrikeResult { outcome   :: Outcome
                                 , boatAfter :: Target
                                 } deriving (Show, Eq)

-- |Target is a rendered ship, set of coordinates
newtype Target = Target (Set Coordinate) deriving (Show, Eq)

-- |Clearance is a set of points surrounding targets
newtype Clearance = Clearance (Set Coordinate) deriving (Show, Eq)

-- |Coordinate is (x,y)
newtype Coordinate = Coordinate (Int,Int) deriving (Show, Eq, Ord)

-- |Create free-form boat or clearance, used in unit tests.
freeform :: [(Int,Int)] -> Set Coordinate
freeform xs = fromList $ map Coordinate xs

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
clearance :: Target -> Clearance
clearance (Target s) = Clearance $ S.unions [nudge n s | n <- [(0,1), (0,-1), (1,0), (-1,0)]] \\ s
