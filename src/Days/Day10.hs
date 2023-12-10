module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.Attoparsec.Text
import           Data.Foldable.WithIndex (FoldableWithIndex (ifoldl))
import           Data.List
import           Data.Map.Strict         (Map, (!))
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Text               (Text, unpack)
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vec
import           Data.Void
import           Debug.Trace             (trace)
import           Linear.V2               (V2 (..))
import qualified Program.RunDay          as R (Day, runDay)
import qualified Util.Util               as U
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = takeWhile1 (/= '\n') `sepBy` endOfLine

------------ TYPES ------------
type Input = [Text]

type Coords = V2 Int

type PipeMap = Map Coords Tile

data Tile
  = Ground
  | Start
  | Pipe Cardinal Cardinal
  deriving (Eq, Show)

data Cardinal = North | South | East | West
  deriving (Eq, Show)

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA input = step firstPipe 1 `div` 2
 where
  pipeMap = mankeloi input
  (start, _) = Map.elemAt 0 $ Map.filter (== Start) pipeMap
  firstPipe = findFirstPipe pipeMap (trace ("start" ++ show start) start)

  -- go around the pipe recursively till back at start.
  step (dir, coords) n =
    if tile == Start
      then n
      else step (newDir, newCoords) (n + 1)
   where
    tile = pipeMap ! trace ("coords:" ++ show coords) coords
    newDir =
      case tile of
        Pipe a b -> getNewDir dir a b
        _        -> error "not a pipe"

    getNewDir dir a b
      | dir == North = if a /= South then a else b
      | dir == South = if a /= North then a else b
      | dir == East = if a /= West then a else b
      | a /= East = a
      | otherwise = b

    (V2 x y) = coords
    newCoords =
      case newDir of
        North -> V2 x (y - 1)
        South -> V2 x (y + 1)
        East  -> V2 (x + 1) y
        West  -> V2 (x - 1) y

-- which pipe connects to the start?
findFirstPipe :: PipeMap -> Coords -> (Cardinal, Coords)
findFirstPipe pipeMap (V2 x y) =
  ( \(dir, coords) ->
      case dir of
        North -> (South, coords)
        South -> (North, coords)
        East  -> (West, coords)
        West  -> (East, coords)
  )
    ( fromJust
        ( find
            isConnected
            [ (South, V2 x (y - 1))
            , (East, V2 (x + 1) y)
            , (North, V2 x (y + 1))
            , (West, V2 (x - 1) y)
            ]
        )
    )
 where
  isConnected :: (Cardinal, Coords) -> Bool
  isConnected (cardinal, coords) =
    case trace ("tile " ++ show tile) tile of
      Just (Pipe a b) -> a == cardinal || b == cardinal
      _               -> False
   where
    tile = Map.lookup coords pipeMap

mankeloi :: Input -> PipeMap
mankeloi =
  ifoldl
    ( \y acc line ->
        ifoldl
          ( \x acc' c ->
              let
                tile =
                  case c of
                    '|' -> Pipe North South
                    '-' -> Pipe East West
                    'L' -> Pipe North East
                    'J' -> Pipe North West
                    '7' -> Pipe South West
                    'F' -> Pipe South East
                    '.' -> Ground
                    'S' -> Start
                    c   -> error "Not a tile"
               in
                Map.insert (V2 x y) tile acc'
          )
          acc
          (unpack line)
    )
    Map.empty

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
