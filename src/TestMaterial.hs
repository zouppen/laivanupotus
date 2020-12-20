module TestMaterial where

import Types
import RuleBook
import Engine

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

-- A valid game
game1 :: LayoutMonad Game
game1 = createGame (Rules teleBoard shipsetFin adjacentKeepout) [boat5v, boat4h, boat3w, boat3h, boat2h, boat1]
