module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import           Control.Applicative  (empty)
import           Data.List
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import qualified Util.Util            as U

import           Data.Attoparsec.Text
import           Data.Void
import           Debug.Trace          (trace)
import qualified Program.RunDay       as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "seeds: "
  seeds <- decimal `sepBy1` space
  string "\n\n"
  seedMaps <- parseMap `sepBy1` string "\n\n"
  pure Input{seeds, seedMaps}

parseMap :: Parser [SeedMap]
parseMap = do
  skipWhile (/= ' ')
  string " map:\n"
  parseSeedMap `sepBy1` "\n"

parseSeedMap :: Parser SeedMap
parseSeedMap = do
  nums <- decimal `sepBy` char ' '
  case nums of
    [destRangeStart, srcRangeStart, rangeLength] ->
      let offset = destRangeStart - srcRangeStart
       in pure SeedMap{offset, srcRangeStart, rangeLength}
    _ ->
      empty

------------ TYPES ------------
data Input = Input
  { seeds    :: [Int]
  , seedMaps :: [[SeedMap]]
  }
  deriving (Show)

data SeedMap = SeedMap
  { offset        :: Int
  , srcRangeStart :: Int
  , rangeLength   :: Int
  }
  deriving (Show)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA Input{seeds, seedMaps} = minimum $ map (getLocation seedMaps) seeds

getLocation :: [[SeedMap]] -> Int -> Int
getLocation seedMaps seed =
  foldl
    getNewDest
    seed
    seedMaps

getNewDest :: Int -> [SeedMap] -> Int
getNewDest seed sMap =
  fromMaybe
    seed
    ( do
        SeedMap{offset} <- find (\r -> r.srcRangeStart <= seed && seed <= r.srcRangeStart + r.rangeLength) sMap
        pure $ seed + offset
    )

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
