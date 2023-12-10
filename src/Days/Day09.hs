module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
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
inputParser = (signed decimal `sepBy1` char ' ') `sepBy` endOfLine

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA =
  foldl
    ( \acc x ->
        let withDiffs = x : getDiffs x
            next =
              foldr
                (\line acc -> last line + acc)
                0
                withDiffs
         in acc + next
    )
    0

getDiffs :: [Int] -> [[Int]]
getDiffs x =
  if all (== 0) d
    then [d]
    else d : getDiffs d
 where
  d = zipWith (flip (-)) x (tail x)

------------ PART B ------------
partB :: Input -> OutputB
partB =
  foldl
    ( \acc x ->
        let withDiffs = x : getDiffs x
            next =
              foldr
                (\line acc -> head line - acc)
                0
                withDiffs
         in acc + next
    )
    0
