{-# LANGUAGE RecordWildCards #-}
module Boat where

data BoatOrientation = Horizontal | Vertical deriving (Show, Eq)

data Boat = Boat { boatX           :: Int
                 , boatY           :: Int
                 , boatLength      :: Int
                 , boatOrientation :: BoatOrientation
                 } deriving (Show, Eq)

newtype RenderedBoat = RenderedBoat [Coordinate] deriving (Show, Eq)

newtype Coordinate = Coordinate (Int,Int) deriving (Show, Eq)

-- |Create free-form boat, used in unit tests.
freeformBoat :: [(Int,Int)] -> RenderedBoat
freeformBoat xs = RenderedBoat $ map Coordinate xs

-- |Render a boat from boat definition
renderBoat :: Boat -> RenderedBoat
renderBoat Boat{..} = RenderedBoat $ case boatOrientation of
  Horizontal -> [ Coordinate (x , boatY) | x <- take boatLength [boatX..]]
  Vertical   -> [ Coordinate (boatX , y) | y <- take boatLength [boatY..]]
