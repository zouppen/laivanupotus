module Engine.Internal where

import Data.Set (Set, unions, member)
import Data.List (sort)

-- |Target is a rendered ship, set of coordinates
data Target = Target { coordinates :: Set Coordinate
                     , clearance   :: Set Coordinate
                     } deriving (Show, Eq)

-- |Coordinate is (x,y)
newtype Coordinate = Coordinate (Int,Int) deriving (Show, Eq, Ord)

newtype Shipset = Shipset [Int] deriving (Show, Eq, Ord)

-- |Keepout zone
newtype KeepoutZone = KeepoutZone [(Int,Int)] deriving (Show)

-- |Create own shipset by defining ship counts
mkShipset :: [Int] -> Shipset
mkShipset ss = Shipset $ sort ss

-- |Dump ship lengths in descending order
dumpShipsetDesc :: Shipset -> [Int]
dumpShipsetDesc (Shipset ss) = reverse ss

-- |Used in rendering.
shipLookup :: [Target] -> Coordinate -> Bool
shipLookup ts = flip member (unions $ map coordinates ts)
