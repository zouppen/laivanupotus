{-# LANGUAGE RecordWildCards #-}
-- |Text mode game, useful for showing the game states
module Main where

import Control.Monad.Except
import Data.IORef
import Data.Map.Strict ((!?))
import Text.Printf
import System.Random (newStdGen, StdGen)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Char (toUpper)
import Data.Void (Void)
import qualified Control.Monad.State.Lazy as S

import Engine
import Types
import NewGame (newGame)
import RuleBook

renderToText :: Bool -> Game -> String
renderToText showBoats game = "  " <> mconcat [ (' ':letterify x) | x <- xs] <> "\n" <>
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
        letterify x = [toEnum $ x + fromEnum 'Ａ']
        emoji x = case x of
          Nothing   -> "  "
          Just Hit  -> "🔥"
          Just Miss -> "🌊"
          Just Sink -> "💀"
        render coord = if showBoats && shipLookup targets coord
                       then "🚢"
                       else emoji $ history !? coord

type Parser = Parsec Void String
type World m = S.StateT (Game, StdGen) m

-- |Parse coordinate in character-number format (such as "B5")
parseCoord :: Board -> Parser Coordinate
parseCoord Board{..} = do
  xRaw <- letterChar
  x <- maybe (fail "Invalid x coordinate") pure $ testEnum 'A' minX maxX xRaw <|> testEnum 'a' minX maxX xRaw
  space
  yRaw <- decimal
  y <- maybe (fail "Invalid y coordinate") pure $ testBounds minY maxY yRaw
  pure $ Coordinate (x,y)

commandParser :: Monad m => Board -> Parser (World m String)
commandParser board = do
  space
  out <- worldStrike <$> try (parseCoord board) <|>
         cmd 'h' (pure helpText) <|>
         cmd 'n' worldNew <|>
         cmd 'p' (worldPrint False) <|>
         cmd 'c' (worldPrint True)
  space
  eof
  pure out
  where cmd c v = try (simple c) >> pure v
        simple :: Char -> Parser ()
        simple c = void $ char c <|> char (toUpper c)

main :: IO ()
main = do
  initialState <- do
    gen <- newStdGen
    pure $ S.runState (newGame teleGameDef) gen
  void $ flip S.runStateT initialState $ forever $ do
    lift $ putStr "> "
    line <- lift $ getLine
    (Game{..}, _) <- S.get
    case parse (commandParser gBoard) "Parse error" line of
      Left e    -> lift $ putStrLn $ errorBundlePretty e
      Right cmd -> do
        out <- cmd
        lift $ putStrLn out

testEnum :: (Enum a) => a -> Int -> Int -> a -> Maybe Int
testEnum start min max c = testBounds min max val
  where diff = (fromEnum c) - (fromEnum start)
        val = diff + min

testBounds :: Int -> Int -> Int -> Maybe Int
testBounds min max x = if x > max || x < min
                       then Nothing
                       else Just x

worldStrike :: Monad m => Coordinate -> World m String
worldStrike coord = do
  (old,gen) <- S.get
  case run old $ strike coord of
    (Left e, _) -> pure $ show e
    (Right a, new) -> do
      S.put (new, gen)
      pure $ show a

worldNew :: Monad m => World m String
worldNew = do
  (_,gen) <- S.get
  S.put $ S.runState (newGame teleGameDef) gen
  return "Started new game"

worldPrint :: Monad m => Bool -> World m String
worldPrint cheat = do
  (game,_) <- S.get
  pure $ renderToText cheat game

helpText =
  "Tervetuloa laivanupotukseen!\n\n\
  \h\tNäytä tämä ohje\n\
  \n\tAloita uusi peli\n\
  \xy\tHyökkää annettuun koordinaattiin (esim. A4)\n\
  \p\tTulosta pelitilanne\n\
  \c\tHuijaa!\n"
