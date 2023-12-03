module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import           Control.Applicative  (optional, (<|>))
import           Data.Functor         (($>))
import           Data.List            (find)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (catMaybes, fromMaybe)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import qualified Util.Util            as U

import           Data.Attoparsec.Text (Parser, decimal, endOfLine, sepBy, space,
                                       string)
import           Data.Void            ()
import qualified Program.RunDay       as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy` endOfLine

parseLine :: Parser Game
parseLine = do
  string "Game "
  gameId <- decimal
  string ": "
  rounds <- parseRound `sepBy` "; "
  pure $ Game{gameId, rounds}

parseRound = do
  a <- optional parseBall
  b <- optional parseBall
  c <- optional parseBall
  let
    colors = catMaybes [a, b, c]

    red = findColor "red"
    green = findColor "green"
    blue = findColor "blue"

    findColor color = maybe 0 fst (find (\(_, c) -> c == color) colors)

  pure Round{red, green, blue}

parseBall = do
  n <- decimal
  space
  color <- string "red" <|> string "green" <|> string "blue"
  optional $ string ", "
  pure (n, color)

------------ TYPES ------------
type Input = [Game]

data Game = Game {gameId :: Int, rounds :: [Round]}
deriving instance Show Game

data Round = Round {red :: Int, green :: Int, blue :: Int}
deriving instance Show Round

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA =
  foldl
    ( \acc game ->
        if isLegal game
          then acc + gameId game
          else acc
    )
    0

isLegal :: Game -> Bool
isLegal Game{rounds} =
  all
    (\Round{red, green, blue} -> red <= 12 && green <= 13 && blue <= 14)
    rounds

------------ PART B ------------
partB :: Input -> OutputB
partB =
  foldl
    (\acc game -> acc + getPower game)
    0

getPower :: Game -> Int
getPower Game{rounds} =
  let
    red = maximum $ map (\r -> r.red) rounds
    green = maximum $ map (\r -> r.green) rounds
    blue = maximum $ map (\r -> r.blue) rounds
   in
    red * green * blue
