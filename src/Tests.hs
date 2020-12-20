{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Except (MonadError, ExceptT, runExcept, runExceptT, catchError)
import Data.Set (Set,fromList)
import Data.List
import Test.HUnit
import Data.Either

import Types
import RuleBook
import Engine
import Engine.Base
import Engine.Internal

-- Helper functions not used outside tests

-- |Create free-form boat or clearance
freeform :: [(Int,Int)] -> Set Coordinate
freeform xs = fromList $ map Coordinate xs

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

boat3h = Boat { boatX = 0
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

boat3x = Boat { boatX = 7
              , boatY = 2
              , boatLength = 3
              , boatOrientation = Vertical
              }

boat4out = Boat { boatX = 7
                , boatY = 7
                , boatLength = 4
                , boatOrientation = Vertical
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
  where hitter remBoat (xy,expect) = toTuple expect $ strikeTarget (Coordinate xy) remBoat
        toTuple expect StrikeTargetResult{..} = (boatAfter, TestCase (assertEqual ("Testing "++show boat) expect outcome))

-- Clearance area test, single boat

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
          (clearance adjacentKeepout $ renderBoat boat)

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

-- Ship count test

testShipCount = TestList
  [ True ~=? checkShipCount shipsetFin [boat5v, boat4h, boat3v, boat3w, boat1, boat2h]
  , False ~=? checkShipCount shipsetFin [boat5v, boat4h, boat3v, boat3w, boat2h]
  , False ~=? checkShipCount shipsetFin [boat5v, boat4h, boat3v, boat3w, boat3h, boat2h]
  ]

checkOverlap' = checkOverlap . map renderBoat

overlapTestSubjects = TestList
  [ True ~=? checkOverlap' []
  , True ~=? checkOverlap' [boat5v]
  , False ~=? checkOverlap' [boat5v, boat4h, boat3x, boat3w, boat2h]
  , False ~=? checkOverlap' [boat5v, boat4h, boat3w, boat3w, boat2h]
  , True ~=? checkOverlap' [boat5v, boat4h, boat3w, boat3h, boat2h, boat1] -- Totally valid game
  ]

-- Clearances

checkClear' = checkClearance adjacentKeepout . map renderBoat
checkClearFull = checkClearance fullKeepout . map renderBoat

testClearancesFull = TestList
  [ True ~=? checkClear' []
  , True ~=? checkClear' [boat5v]
  , True ~=? checkClear' [boat5v, boat4h, boat3w, boat3h, boat2h, boat1] -- Totally valid game
  , False ~=? checkClear' [boat5v, boat4h, boat3v, boat3h, boat2h, boat1]
  , True ~=? checkClear' [boat5v, boat4h, boat3w, boat3w, boat2h] -- Invalid boat overlap but valid clearance
  , False ~=? checkClearFull [boat4h, boat3w] -- Full clearout should fail. Are diagonally positioned
  , True ~=? checkClearFull [boat2h, boat3w] -- Full clearout, are one position off
  ]

-- Whole board test

testAll = runExcept . createGame (Rules teleBoard shipsetFin adjacentKeepout)

testAllRules = TestList
  [ True ~=? isRight (testAll [boat5v, boat4h, boat3w, boat3h, boat2h, boat1]) -- Totally valid game
  , Left Overlapping ~=? testAll [boat5v, boat4h, boat3w, boat3w, boat2h] -- Invalid boat overlap but valid clearance
  , Left CountMismatch ~=? testAll []
  , Left OutOfBounds ~=? testAll [boat4out]
  , Left TooClose ~=? testAll [boat5v, boat4h, boat3v, boat3h, boat2h, boat1] -- Too close of another ship
  ]

-- Gameplay

type TestWriter = Writer [Test]

storeTest :: Assertion -> TestWriter ()
storeTest a = tell [TestCase a]

-- W T F. Never give monads to this guy.
catchAndReport
  :: (MonadError e (t1 (t2 (WriterT [a1] m))), MonadTrans t1,
      MonadTrans t2, Monad m, Monad (t2 (WriterT [a1] m))) =>
     (Either e a2 -> a1)
     -> t1 (t2 (WriterT [a1] m)) a2 -> t1 (t2 (WriterT [a1] m)) ()
catchAndReport testify act = eitherify act >>= \x -> lift $ lift $ tell [testify x]

strikeTest :: (Either StrikeFail Outcome -> Test)
           -> (Int, Int)
           -> StrikeMonad TestWriter ()
strikeTest testify = catchAndReport testify . strike . Coordinate

-- |Run given action and catch error to Either type. For some reason
-- MonadErorr doesn't have `try`.
eitherify :: MonadError e m => m a -> m (Either e a)
eitherify act = (Right <$> act) `catchError` (pure . Left)

writerToTest :: TestWriter a -> Test
writerToTest = TestList . execWriter

testGame1 :: Test
testGame1 = writerToTest $ do
  case runExcept $ createGame (Rules teleBoard shipsetFin adjacentKeepout) [boat5v, boat4h, boat3w, boat3h, boat2h, boat1] of
    Left e -> storeTest $ assertFailure $ "Unable to create testGame boats: " ++ show e
    Right game -> do
      flip runStateT game . runExceptT $ do
        strikeTest (Right Miss ~=?) (3,2) -- Normal miss condition
        strikeTest (Right Sink ~=?) (0,0) -- Single-block ship
        strikeTest (Left InvalidCoordinate ~=?) (10,10)
        strikeTest (Left AlreadyHit ~=?) (0,0) -- Already sunken ship
        strikeTest (Left AlreadyHit ~=?) (3,2) -- Coordinate already missed
        strikeTest (Left InvalidCoordinate ~=?) (10,10) -- Retrying same invalid coordinate again
        -- Sinking longer ship
        strikeTest (Right Hit ~=?) (1,9)
        strikeTest (Right Hit ~=?) (0,9)
        strikeTest (Right Sink ~=?) (2,9)
        strikeTest (Left AlreadyHit ~=?) (0,9) -- And then rehit it
        -- Hitting outside of each direction
        strikeTest (Left InvalidCoordinate ~=?) (5,-1) -- Up
        strikeTest (Left InvalidCoordinate ~=?) (3,10) -- Right
        strikeTest (Left InvalidCoordinate ~=?) (10,7) -- Down
        strikeTest (Left InvalidCoordinate ~=?) (-1,2) -- Left
        strikeTest (Left InvalidCoordinate ~=?) (13,2) -- Way off
      pure () -- Throw final state away

-- Group all tests

tests = TestList [ testBoatCreation
                 , testStrikes
                 , testClearances
                 , testBounds
                 , testShipCount
                 , overlapTestSubjects
                 , testClearancesFull
                 , testAllRules
                 , testGame1
                 ]

main = runTestTT tests
