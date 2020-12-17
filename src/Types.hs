module Types where

import Data.Map.Strict (Map)

import Engine.Internal (Target, Shipset, KeepoutZone, Coordinate)

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

data Board = Board { minX :: Int
                   , minY :: Int
                   , maxX :: Int
                   , maxY :: Int
                   } deriving (Show, Eq)

data LayoutFailure = OutOfBounds | Overlapping | TooClose | CountMismatch deriving (Show, Eq)

-- |Rules of the game. See Rulebook.hs for some quite common setups
data Rules = Rules { board   :: Board       -- ^Game are boundaries
                   , shipset :: Shipset     -- ^Number of ships and their sizes
                   , keepout :: KeepoutZone -- ^Keepout zone, if it's adjacent or also diagonal
                   } deriving (Show)

-- |Game state. Contains enough information to render game state on screen
data Game = Game { gBoard   :: Board                  -- ^Board settings
                 , targets  :: [Target]               -- ^Target list
                 , history  :: Map Coordinate Outcome -- ^Map of strikes
                 } deriving (Show, Eq)
