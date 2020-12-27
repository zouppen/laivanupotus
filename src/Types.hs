module Types ( Board(..)
             , Boat(..)
             , BoatOrientation(..)
             , Game(..)
             , LayoutFailure(..)
             , Outcome(..)
             , Rules(..)
             , StrikeFail(..)
             -- Re-exports
             , Coordinate(..)
             , KeepoutZone
             , Shipset
             , Target
             , shipLookup
             , dumpShipsetDesc
             ) where

import Data.Map.Strict (Map)

import Engine.Internal

data BoatOrientation = Horizontal | Vertical deriving (Show, Eq)

data Outcome = Miss  -- ^Target missed
             | Hit   -- ^Target hit, didn't make it sink.
             | Sink  -- ^Target hit and sink
             | Close -- ^In clearance area. In normal game not exposed before sink.
             deriving (Show, Eq)

data StrikeFail = InvalidCoordinate -- ^Coordinate is not valid.
                | AlreadyHit        -- ^Target is already hit.
                | Exposed           -- ^We know what's there already.
                deriving (Show, Eq)

data Boat = Boat { boatX           :: Int
                 , boatY           :: Int
                 , boatLength      :: Int
                 , boatOrientation :: BoatOrientation
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
