{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.List
import Test.HUnit
import Boat

-- Maritime mania -- Test boats for everybody --

boat5v = Boat { boatX = 9
              , boatY = 5
              , boatLength = 5
              , boatOrientation = Vertical
              }

boat4h = Boat { boatX = 5
              , boatY = 3
              , boatLength = 4
              , boatOrientation = Horizontal
              }

-- Boat too close to boat4h
boat3v = Boat { boatX = 4
              , boatY = 1
              , boatLength = 3
              , boatOrientation = Vertical
              }

boat3w = Boat { boatX = 4
              , boatY = 4
              , boatLength = 3
              , boatOrientation = Vertical
              }

boat3h = Boat { boatX = 1
              , boatY = 9
              , boatLength = 3
              , boatOrientation = Horizontal
              }

boat2h = Boat { boatX = 1
              , boatY = 4
              , boatLength = 2
              , boatOrientation = Horizontal
              }

boat1 = Boat { boatX = 0
             , boatY = 0
             , boatLength = 1
             , boatOrientation = Horizontal
             }

-- Boat creation tests

boatPairs = [(boat1, [(0,0)])
            ,(boat4h, [(5,3), (6,3), (7,3), (8,3)])
            ,(boat3v, [(4,1), (4,2), (4,3)])
            ]

testBoatCreation = TestList $ map test boatPairs
  where test (boat,reference) = TestCase $ assertEqual (show boat) (Target $ freeform reference) (renderBoat boat)

-- Boat hit tests

strikes = [ (boat1, [ ((0,0), Sink)
                    , ((4,4), Miss)
                    ])
          , (boat1, [ ((0,1), Miss)
                    ])
          , (boat3v, [ ((4,1), Hit)
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
             , (boat4h, [(5,2)
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

bounds = [ (boat1, True, Board{ minX = 0
                              , minY = 0
                              , maxX = 0
                              , maxY = 0
                              })
         , (boat3v, True, Board{ minX = 4
                               , minY = 1
                               , maxX = 4
                               , maxY = 3
                               })
         , (boat3v, False, Board{ minX = 0
                                , minY = 0
                                , maxX = 9
                                , maxY = 2
                                })
         ]

testBounds = TestList $ map toCase bounds
  where toCase (boat, expected, board) = TestCase $
          (assertEqual $ "Boundaries of " ++ show boat ++ " and "  ++ show board)
          expected
          (checkBoundary board $ renderBoat boat)
       
tests = TestList [ testBoatCreation
                 , testStrikes
                 , testClearances
                 , testBounds
                 ]

main = runTestTT tests
