{-# LANGUAGE RecordWildCards #-}
module Main where

import Test.HUnit
import Boat

-- Boat creation tests
boat1 = Boat { boatX = 0
             , boatY = 0
             , boatLength = 1
             , boatOrientation = Horizontal
             }

boatH = Boat { boatX = 5
             , boatY = 3
             , boatLength = 4
             , boatOrientation = Horizontal
             }

boatV = Boat { boatX = 4
             , boatY = 1
             , boatLength = 3
             , boatOrientation = Vertical
             }

boatPairs = [(boat1, [(0,0)])
            ,(boatH, [(5,3), (6,3), (7,3), (8,3)])
            ,(boatV, [(4,1), (4,2), (4,3)])
            ]

testBoatCreation = TestList $ map test boatPairs
  where test (boat,reference) = TestCase $ assertEqual (show boat) (freeformBoat reference) (renderBoat boat)

tests = testBoatCreation

main = runTestTT tests
