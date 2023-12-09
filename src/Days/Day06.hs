module Days.Day06 (runDay) where

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

import           Control.Applicative  (Alternative (many))
import           Data.Attoparsec.Text
import           Data.Void
import           GHC.Float            (ceilingFloat)
import qualified Program.RunDay       as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  string "Time:"
  many space
  times <- decimal `sepBy1` many space
  string "\nDistance:"
  many space
  distances <- decimal `sepBy1` many space
  pure $ zipWith (\time record -> Race{time, record}) times distances

------------ TYPES ------------
type Input = [Race]

data Race = Race {time :: Int, record :: Int} deriving (Show)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  product $ map solveRecord input

solveRecord :: Race -> Int
solveRecord Race{time, record} =
  -- d = x * (time - x)
  let
    t :: Float
    t = fromIntegral time
    y :: Float
    y = fromIntegral record + 1
    a = sqrt $ (t ^ 2) - (4.0 * y)
    x1 = -((-t) + a) / 2
    x2 = -((-t) - a) / 2
   in
    (floor x2) - ceilingFloat x1 + 1

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
