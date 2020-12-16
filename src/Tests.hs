{-# LANGUAGE RecordWildCards #-}
module Main where

import Test.HUnit
import Boat

-- Boat creation tests
boatPairs = [
  ( Boat { boatX = 0
         , boatY = 0
         , boatLength = 1
         , boatOrientation = Horizontal
         } ,
    [ (0,0) ]
  ), ( Boat { boatX = 5
            , boatY = 3
            , boatLength = 4
            , boatOrientation = Horizontal
            } ,
       [ (5,3), (6,3), (7,3), (8,3) ]
  ), ( Boat { boatX = 4
            , boatY = 1
            , boatLength = 3
            , boatOrientation = Vertical
            } ,
       [ (4,1), (4,2), (4,3) ]
     )
  ]

testBoatCreation = TestList $ map test boatPairs
  where test (boat,result) = TestCase $ assertEqual (show boat) (RenderedBoat result) (renderBoat boat)

tests = testBoatCreation

main = runTestTT tests
