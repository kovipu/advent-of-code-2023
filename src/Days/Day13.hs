module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.List
import qualified Data.List               as List
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Set                (Set, member)
import qualified Data.Set                as Set
import           Data.Text               (Text)
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vec
import qualified Util.Util               as U

import           Data.Attoparsec.Text
import           Data.Foldable.WithIndex (ifoldl)
import           Data.Void
import           Debug.Trace             (traceShow)
import qualified Program.RunDay          as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (notChar '\n') `sepBy1` endOfLine `sepBy1` string "\n\n"

------------ TYPES ------------
type Input = [[String]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- partA :: Input -> OutputA
partA = sum . mapMaybe (findReflection . intoMap)

data Note = Note
  { borked :: Map (Int, Int) Bool
  , width  :: Int
  , height :: Int
  }
  deriving (Show)

intoMap :: [String] -> Note
intoMap note = Note{borked, width, height}
 where
  borked =
    ifoldl
      ( \y acc line ->
          ifoldl
            (\x acc' c -> Map.insert (x, y) (c == '#') acc')
            acc
            line
      )
      Map.empty
      note
  width = length $ head note
  height = length note

findReflection :: Note -> Maybe Int
findReflection Note{borked, width, height} =
  case (yReflection, xReflection) of
    (Just y, _)        -> Just $ 100 * y
    (Nothing, Just x)  -> Just x
    (Nothing, Nothing) -> Nothing
 where
  xReflection = find findX [1 .. width]
  yReflection = find findY [1 .. height]

  -- does n column reflect?
  findX n =
    -- a column reflects if for each x < n, a mirror image exists at (2n - x - 1)
    not (null (lg filtered)) && all (\((x, y), v) -> Map.lookup (lg n + n - x - 1, y) borked == Just v) (Map.assocs filtered)
   where
    -- filter out >= n and the ones that are farther from n than the thing is wide
    filtered = Map.filterWithKey (\(x, y) _ -> (n - (width - n) - 1) < x && x < n) borked

  findY n =
    not (null filtered) && all (\((x, y), v) -> Map.lookup (x, n + n - y - 1) borked == Just v) (Map.assocs filtered)
   where
    filtered = Map.filterWithKey (\(x, y) _ -> (n - (height - n) - 1) < y && y < n) borked

lg v = traceShow v v

------------ PART B ------------
-- partB :: Input -> OutputB
-- partB = error "not implemented"
partB = map (fixAndFindReflection . intoMap)

fixAndFindReflection :: Note -> Int
fixAndFindReflection = recurseReflection . fixSmudges
 where
  recurseReflection (note : xs) =
    case findReflection (lg note) of
      Just s  -> s
      Nothing -> recurseReflection xs

fixSmudges :: Note -> [Note]
fixSmudges Note{borked, width, height} = map fixSmudge [0 ..]
 where
  fixSmudge n =
    let
      x = n `mod` width
      y = n `div` width
      newBorked = Map.update (\n -> if n then Just False else Just True) (x, y) borked
     in
      Note{borked = newBorked, width, height}
