{-# LANGUAGE RecordWildCards #-}
-- |Text mode game, useful for showing the game states
module Textmode where

import Control.Monad.Except
import Data.IORef
import Data.Map.Strict ((!?))
import Text.Printf
import System.IO.Unsafe (unsafePerformIO)

import Engine
import Types
import TestMaterial

renderToText :: Bool -> Game -> String
renderToText showBoats game = "  " <> mconcat [ printf "%3d" x | x <- xs] <> "\n" <>
                    separator "┌" "┬" "┐" <>
                    mconcat [ row y | y <- [minY..maxY]] <>
                    separator "└" "┴" "┘"
  where Game{..}  = game
        Board{..} = gBoard
        xs = [minX..maxX]
        row y = printf "%2d│" y <> mconcat [status $ Coordinate (x,y) | x <- xs] <> "\n" <>
                if y == maxY then "" else separator "├" "┼" "┤"
        status coord = render coord <> "│"
        separator start mid end = "  " <> start <> mconcat [ "──" <> sep | x <- xs, let sep = if x == maxX then end else mid ] <> "\n"
        emoji x = case x of
          Nothing -> "  "
          Just Hit -> "🔥"
          Just Miss -> "🌊"
          Just Sink -> "💀"
        render coord = if showBoats && shipLookup targets coord
                       then "🚢"
                       else emoji $ history !? coord

-- And now comes the global state hack

myGame :: IORef Game
{-# NOINLINE myGame #-}
myGame = unsafePerformIO $ newGame >>= newIORef 
 where newGame = case runExcept game1 of
         Left e -> fail $ "Error creating game: " ++ show e
         Right x -> pure x

s :: Int -> Int -> IO ()
s x y = do
  old <- readIORef myGame
  new <- case run old $ strike $ Coordinate (x,y) of
    (Left e, _) -> fail $ show e
    (Right a, new) -> print a >> pure new
  writeIORef myGame new

p = printGame False
c = printGame True

printGame cheat = do
  game <- readIORef myGame
  putStr $ renderToText cheat game

h = do
  putStrLn $
    "Tervetuloa laivanupotukseen!\n\n\
    \h\tNäytä tämä ohje\n\
    \n\tAloita uusi peli\n\
    \s x y\tHyökkää annettuun koordinaattiin\n\
    \p\tTulosta pelitilanne\n\
    \c\tHuijaa!\n"