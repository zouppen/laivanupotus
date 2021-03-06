-- Some common configurable rules for a game.
module RuleBook where

import Types
import Engine.Internal (mkShipset, KeepoutZone(..))

-- |Finnish style https://fi.wikipedia.org/wiki/Laivanupotus
shipsetFin     = mkShipset [1, 2, 3, 3, 4, 5]

-- |Bradley rules https://en.wikipedia.org/wiki/Battleship_(game)
shipsetBradley = mkShipset [2, 3, 3, 4, 5]    

-- |Board played on the telephone (keys 0-9, not 1-10)
teleBoard = Board { minX = 0
                  , minY = 0
                  , maxX = 9
                  , maxY = 9
                  }

-- |Normal kids playground
classicBoard = Board { minX = 1
                     , minY = 1
                     , maxX = 10
                     , maxY = 10
                     }

-- |Adjacent keepout zone. The most common rule. The boats may not
-- touch each other, but diagonal placement allowed
adjacentKeepout = KeepoutZone [(0,1), (0,-1), (1,0), (-1,0)]

-- |Full keepout. The boats must not touch even diagonally.
fullKeepout = KeepoutZone [(0,1), (0,-1), (1,0), (-1,0), (-1,-1), (-1,1), (1,1), (1,-1)]

-- |Our favourite game ruleset
teleGameDef :: Rules
teleGameDef = Rules teleBoard shipsetFin adjacentKeepout
