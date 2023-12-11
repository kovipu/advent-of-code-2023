module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import           Control.Applicative  (empty)
import           Data.Function        (on)
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
import qualified Data.Foldable        as Foldable
import           Data.List.Split      (chunksOf)
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
       in pure SeedMap{offset, destRangeStart, srcRangeStart, rangeLength}
    _ ->
      empty

------------ TYPES ------------
data Input = Input
  { seeds    :: [Int]
  , seedMaps :: [[SeedMap]]
  }
  deriving (Show)

data SeedMap = SeedMap
  { offset         :: Int
  , destRangeStart :: Int
  , srcRangeStart  :: Int
  , rangeLength    :: Int
  }
  deriving (Show)

type OutputA = Int

type OutputB = Int

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
        SeedMap{offset} <- find (\r -> r.srcRangeStart <= seed && seed < r.srcRangeStart + r.rangeLength) sMap
        pure $ seed + offset
    )

------------ PART B ------------
partB :: Input -> OutputB
partB Input{seeds, seedMaps} =
  let
    newSeeds = (\[start, len] -> (start, start + len - 1)) <$> chunksOf 2 seeds
    seedExists s = isJust $ find (\(start, end) -> start <= s && s <= end) newSeeds
   in
    -- start from smallest range -> go up layer by layer -> is there a corresponding seed?
    fromJust $ find (seedExists . getSeedStart seedMaps) [0 ..]

getSeedStart :: [[SeedMap]] -> Int -> Int
getSeedStart seedMaps seed =
  foldr
    getStart
    seed
    seedMaps

-- inverse of getNewDest
getStart :: [SeedMap] -> Int -> Int
getStart sMap seed =
  fromMaybe
    seed
    ( do
        SeedMap{offset} <- find (\r -> r.destRangeStart <= seed && seed < r.destRangeStart + r.rangeLength) sMap
        pure $ seed - offset
    )
