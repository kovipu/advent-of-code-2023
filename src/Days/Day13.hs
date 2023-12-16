module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.List
import qualified Data.List            as List
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import qualified Util.Util            as U

import           Data.Attoparsec.Text
import           Data.Void
import           Debug.Trace          (traceShow)
import qualified Program.RunDay       as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (notChar '\n') `sepBy1` endOfLine `sepBy1` string "\n\n"

------------ TYPES ------------
type Input = [[String]]

type OutputA = Void

type OutputB = Void

------------ PART A ------------
-- partA :: Input -> OutputA
partA input =
  sum $ map findReflection input

findReflection :: [String] -> Int
findReflection note =
  100 * findOnAxis note + findOnAxis (transpose note)

findOnAxis note =
  fromMaybe 0 $ find (reflection note) [1 .. length note]
 where
  reflection :: [String] -> Int -> Bool
  reflection note n =
    not (null a) && not (null b) && all (uncurry (==)) (zip a b)
   where
    a = reverse $ List.take n note
    b = drop n note

lg v = traceShow v v

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
