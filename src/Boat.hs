{-# LANGUAGE RecordWildCards #-}
module Boat where

data BoatOrientation = Horizontal | Vertical  deriving (Show, Eq)

data Boat = Boat { boatX           :: Int
                 , boatY           :: Int
                 , boatLength      :: Int
                 , boatOrientation :: BoatOrientation
                 } deriving (Show, Eq)

newtype RenderedBoat = RenderedBoat [(Int,Int)] deriving (Show, Eq)

renderBoat :: Boat -> RenderedBoat
renderBoat Boat{..} = RenderedBoat $ case boatOrientation of
  Horizontal -> [ (x , boatY) | x <- take boatLength [boatX..]]
  Vertical   -> [ (boatX , y) | y <- take boatLength [boatY..]]
