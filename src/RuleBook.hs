-- Some common configurable rules for a game.
module RuleBook ( shipsetFin
                , shipsetBradley
                , teleBoard
                , classicBoard
                ) where

import Data.List (sort)
import Types
import Boat.Internal

-- |Finnish style https://fi.wikipedia.org/wiki/Laivanupotus
shipsetFin     = Shipset [1, 2, 3, 3, 4, 5]

-- |Bradley rules https://en.wikipedia.org/wiki/Battleship_(game)
shipsetBradley = Shipset [2, 3, 3, 4, 5]    

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

-- |Create own shipset by defining ship counts
ownShipset :: [Int] -> Shipset
ownShipset ss = Shipset $ sort ss
