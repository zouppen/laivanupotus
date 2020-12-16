{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List
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
  where test (boat,reference) = TestCase $ assertEqual (show boat) (Target $ freeform reference) (renderBoat boat)

-- Boat hit tests

strikes = [ (boat1, [ ((0,0), Sink)
                    , ((4,4), Miss)
                    ])
          , (boat1, [ ((0,1), Miss)
                    ])
          , (boatV, [ ((4,1), Hit)
                    , ((3,3), Miss)
                    , ((5,1), Miss)
                    , ((4,3), Hit)
                    , ((4,1), Miss) -- Rehit? What should be done? Now it's Miss.
                    , ((4,2), Sink)
                    ])
          ]

testStrikes = TestList $ map testStrike strikes

testStrike (boat, shots) = TestList $ snd $ mapAccumL hitter (renderBoat boat) shots
  where hitter remBoat (xy,expect) = toTuple expect $ strike (Coordinate xy) remBoat
        toTuple expect StrikeResult{..} = (boatAfter, TestCase (assertEqual ("Testing "++show boat) expect outcome))

-- Clearance tests

clearances = [ (boat1, [(0,-1)
                       ,(0,1)
                       ,(-1,0)
                       ,(1,-0)
                       ])
             , (boatH, [(5,2)
                       ,(6,2)
                       ,(7,2)
                       ,(8,2)
                       ,(5,4)
                       ,(6,4)
                       ,(7,4)
                       ,(8,4)
                       ,(4,3)
                       ,(9,3)
                       ])
             ]
                       
testClearances = TestList $ map toCase clearances
  where toCase (boat,expected) = TestCase $
          (assertEqual $ "Clearances of " ++ show boat)
          (Clearance $ freeform expected)
          (clearance $ renderBoat boat)

tests = TestList [ testBoatCreation
                 , testStrikes
                 , testClearances
                 ]

main = runTestTT tests
