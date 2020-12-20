{-# LANGUAGE RecordWildCards #-}
-- |Create new game
module NewGame where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Loops (iterateUntil)
import System.Random

import Types
import Engine

-- random-1.2 is not yet very widespread so implementing states with old version

-- |Generate random ship which fits on the board
randomBoat :: RandomGen s => Board -> Int -> State s Boat
randomBoat Board{..} boatLength = do
  boatOrientation <- (\h -> if h then Horizontal else Vertical) <$> state random
  let (highX, highY) = case boatOrientation of
                         Horizontal -> (maxX-boatLength+1, maxY)
                         Vertical   -> (maxX, maxY-boatLength+1)
  boatX <- state $ randomR (minX,highX)
  boatY <- state $ randomR (minY,highY)
  pure Boat{..}

-- |Generate ship of given length until it fits
fitShip :: RandomGen s => Rules -> [Boat] -> Int -> State s Boat
fitShip rules boats boatLength = iterateUntil valid $ randomBoat (board rules) boatLength
  where valid boat = case runExcept $ createGame rules (boat:boats) of
                       Left OutOfBounds   -> error "Generated boat out of bounds. Fatal."
                       Left CountMismatch -> True  -- Do not stress about the count at this point.
                       Right _            -> True  -- Game generated, this is also fine.
                       _                  -> False -- Otherwise let's try again

-- |Fits all ships to the playground according to the rules.
fitShips :: RandomGen s => Rules -> State s [Boat]
fitShips rules = foldM f [] $ dumpShipsetDesc $ shipset rules
  where f boats len = (:boats) <$> fitShip rules boats len

newGame :: RandomGen s => Rules -> State s Game
newGame rules = do
  boats <- fitShips rules
  case runExcept $ createGame rules boats of
    Left _     -> error "Game generation fails. Fatal"
    Right game -> pure game
