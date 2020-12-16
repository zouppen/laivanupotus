{-# LANGUAGE RecordWildCards #-}
module Boat where

data BoatOrientation = Horizontal | Vertical deriving (Show, Eq)

data Outcome = Miss | Hit | Sink deriving (Show, Eq)

data Boat = Boat { boatX           :: Int
                 , boatY           :: Int
                 , boatLength      :: Int
                 , boatOrientation :: BoatOrientation
                 } deriving (Show, Eq)

data StrikeResult = StrikeResult { outcome   :: Outcome
                                 , boatAfter :: RenderedBoat
                                 } deriving (Show, Eq)

newtype RenderedBoat = RenderedBoat [Coordinate] deriving (Show, Eq)

newtype Coordinate = Coordinate (Int,Int) deriving (Show, Eq, Ord)

-- |Create free-form boat, used in unit tests.
freeformBoat :: [(Int,Int)] -> RenderedBoat
freeformBoat xs = RenderedBoat $ map Coordinate xs

-- |Render a boat from boat definition
renderBoat :: Boat -> RenderedBoat
renderBoat Boat{..} = RenderedBoat $ case boatOrientation of
  Horizontal -> [ Coordinate (x , boatY) | x <- take boatLength [boatX..]]
  Vertical   -> [ Coordinate (boatX , y) | y <- take boatLength [boatY..]]

-- |Try to hit the boat
strike :: Coordinate -> RenderedBoat -> StrikeResult
strike x (RenderedBoat before) = StrikeResult outcome $ RenderedBoat after
  where after = filter (/= x) before
        outcome = if after == before
                  then Miss
                  else if null after
                       then Sink
                       else Hit

-- |Rectangular clearance, not including diagonal as in some hose
-- rules.
toClearance :: Coordinate -> [Coordinate]
toClearance (Coordinate (x,y)) = [Coordinate (x',y') | x' <- [x-1, x+1], y' <- [y-1, y+1]]
