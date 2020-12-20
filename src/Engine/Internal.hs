module Engine.Internal where

import Data.Set (Set)
import Data.List (sort)

-- |Target is a rendered ship, set of coordinates
newtype Target = Target (Set Coordinate) deriving (Show, Eq)

-- |Clearance is a set of points surrounding targets
newtype Clearance = Clearance (Set Coordinate) deriving (Show, Eq)

-- |Coordinate is (x,y)
newtype Coordinate = Coordinate (Int,Int) deriving (Show, Eq, Ord)

newtype Shipset = Shipset [Int] deriving (Show, Eq, Ord)

-- |Keepout zone
newtype KeepoutZone = KeepoutZone [(Int,Int)] deriving (Show)

-- |Create own shipset by defining ship counts
mkShipset :: [Int] -> Shipset
mkShipset ss = Shipset $ sort ss

-- some helpers
unwrapT (Target a) = a
unwrapC (Clearance a) = a
