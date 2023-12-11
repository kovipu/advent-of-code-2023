module Days.Day11 (runDay) where

{- ORMOLU_DISABLE -}
import           Control.Applicative     (Alternative (many))
import           Data.Attoparsec.Text
import           Data.Foldable.WithIndex
import           Data.List
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vec
import           Data.Void
import           Debug.Trace             (traceShow)
import           Linear                  (V2 (..))
import qualified Program.RunDay          as R (Day, runDay)
import qualified Util.Util               as U
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (notChar '\n') `sepBy` endOfLine

------------ TYPES ------------
type Input = [[Char]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = findDistances . combinations . findGalaxies . expand

lg v = traceShow v v

expand :: Input -> Input
expand = transpose . expandAxis . transpose . expandAxis
 where
  expandAxis =
    foldr
      ( \line acc ->
          if all (== '.') line
            then line : line : acc
            else line : acc
      )
      []

findGalaxies :: Input -> [V2 Int]
findGalaxies =
  ifoldl
    ( \y acc line ->
        ifoldl
          ( \x acc' c ->
              if c == '#'
                then V2 x y : acc'
                else acc'
          )
          acc
          line
    )
    []

combinations l = [(a, b) | (a : as) <- tails l, b <- as]

findDistances :: [(V2 Int, V2 Int)] -> Int
findDistances = foldl (\acc (V2 x1 y1, V2 x2 y2) -> acc + abs (x1 - x2) + abs (y1 - y2)) 0

------------ PART B ------------
partB :: Input -> OutputB
partB input = findDistances' gaps $ combinations $ findGalaxies input
 where
  gaps = findGaps input

findGaps :: Input -> V2 [Int]
findGaps input = V2 xGaps yGaps
 where
  xGaps = gaps $ transpose input
  yGaps = gaps input

  gaps =
    ifoldl
      ( \i acc line ->
          if all (== '.') line then i : acc else acc
      )
      []

findDistances' (V2 xGaps yGaps) = foldl (\acc pair -> acc + distance pair) 0
 where
  distance (V2 x1 y1, V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2) + xm + ym
   where
    mul = 999999 -- off by one to correct the original math.
    xm = mul * calcGaps xGaps x1 x2
    ym = mul * calcGaps yGaps y1 y2
  -- gaps between
  calcGaps gaps a b = length $ filter (\g -> a < g && g < b || b < g && g < a) gaps
