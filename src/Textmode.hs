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
import System.Console.Readline (addHistory, readline)
import Data.Ix (inRange)

import Engine
import Types
import NewGame (newGame)
import RuleBook

renderToText :: Bool -> Game -> String
renderToText showBoats game = "   " <> mconcat [ (' ':letterify x) | x <- xs] <> "\n" <>
                    separator "┌" "┬" "┐" <>
                    mconcat [ row y | y <- [minY..maxY]] <>
                    separator "└" "┴" "┘"
  where Game{..}  = game
        Board{..} = gBoard
        xs = [minX..maxX]
        row y = printf "%2d │" y <> mconcat [status $ Coordinate (x,y) | x <- xs] <> "\n" <>
                if y == maxY then "" else separator "├" "┼" "┤"
        status coord = render coord <> "│"
        separator start mid end = "   " <> start <> mconcat [ "───" <> sep | x <- xs, let sep = if x == maxX then end else mid ] <> "\n"
        letterify x = [' ',toEnum $ x + fromEnum 'A', ' ']
        render coord = if showBoats && shipLookup targets coord
                       then "båt"
                       else emoji $ history !? coord

emoji :: Maybe Outcome -> [Char]
emoji x = case x of
            Nothing    -> "   "
            Just Hit   -> " X "
            Just Miss  -> " ~ "
            Just Sink  -> "*X*"
            Just Close -> " - "

type Parser = Parsec Void String
type World m = S.StateT (Game, StdGen) m

-- |Parse coordinate in character-number format (such as "B5")
parseCoord :: Board -> Parser Coordinate
parseCoord Board{..} = do
  x <- label "x coordinate" $ token (\x -> testEnum 'A' minX maxX x <|> testEnum 'a' minX maxX x) mempty
  hidden space
  y <- label "y coordinate" $ validateAhead (inRange (minY,maxY)) decimal
  pure $ Coordinate (x,y)

-- |Tests the condition while look-aheading. Otherwise works
-- normally. Makes error messages more precise.
validateAhead :: MonadParsec e s m => (b -> Bool) -> m b -> m b
validateAhead f p = do
  x <- lookAhead p
  if f x
    then p
    else failure Nothing mempty

commandParser :: Monad m => Board -> Parser (World m String)
commandParser board = do
  hidden space
  out <- worldStrike <$> try (parseCoord board) <|>
         cmd 'h' (pure helpText) <|>
         cmd 'n' worldNew <|>
         cmd 'p' (worldPrint False) <|>
         cmd 'c' (worldPrint True)
  hidden space
  eof
  pure out
  where cmd c v = try (simple c) >> pure v
        simple :: Char -> Parser ()
        simple c = void $ char c <|> hidden (char (toUpper c))

main :: IO ()
main = do
  initialState <- do
    gen <- newStdGen
    pure $ S.runState (newGame teleGameDef) gen
  putStr helpText
  void $ S.runStateT loop initialState

loop :: World IO ()
loop = do
  mbLine <- lift $ readline "> "
  case mbLine of
    Nothing -> lift $ putStrLn "(quit)"
    Just line -> do
      lift $ addHistory line
      (Game{..}, _) <- S.get
      case parse (commandParser gBoard) "" line of
        Left e    -> lift $ putStr $ cleanError $ errorBundlePretty e
        Right cmd -> do
          out <- cmd
          lift $ putStr out
      loop

-- |Remove file name and the actual user input from the error
-- message. Makes messages easier to understand on console. This might
-- be sensitive to format changes in megaparsec. Tested with
-- megaparsec-7.0.5
cleanError :: String -> String
cleanError s = (drop 2 $ dropline $ dropline $ dropline s) ++ "type 'h' to see help\n"
  where dropline = tail . dropWhile (/= '\n')

-- |Test if given enum is in bounds (in given number characters from 'start').
testEnum :: (Enum a) => a -> Int -> Int -> a -> Maybe Int
testEnum start min max c = if inRange (min,max) val
                           then Just val
                           else Nothing
  where diff = (fromEnum c) - (fromEnum start)
        val = diff + min

worldStrike :: Monad m => Coordinate -> World m String
worldStrike coord = do
  (old,gen) <- S.get
  case run old $ strike coord of
    (Left AlreadyHit, _) -> pure "error: You have already hit that coordinate!\n"
    (Left Exposed, _)    -> pure "error: A ship cannot be there! Too close of a sunken ship.\n"
    (Left e, _)          -> pure $ "error: " ++ show e ++ "\n"
    (Right a, new) -> do
      S.put (new, gen)
      let msg = case a of
                  Hit -> "HIT!"
                  Miss -> "miss."
                  Sink -> "HIT! Target destroyed."
      pure $ "Fire... " ++ msg ++ "\n"

worldNew :: Monad m => World m String
worldNew = do
  (_,gen) <- S.get
  S.put $ S.runState (newGame teleGameDef) gen
  return "Started new game\n"

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
