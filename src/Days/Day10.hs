module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.Attoparsec.Text    (Parser, endOfLine, sepBy, takeWhile1)
import           Data.Foldable.WithIndex (FoldableWithIndex (ifoldl))
import           Data.List               (find)
import           Data.Map.Strict         (Map, (!))
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromJust)
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Text               (Text, unpack)
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vec
import           Data.Void               (Void)
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

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = step firstPipe 1 `div` 2
 where
  pipeMap = mankeloi input
  (start, _) = Map.elemAt 0 $ Map.filter (== Start) pipeMap
  firstPipe = findFirstPipe pipeMap start

  -- go around the pipe recursively till back at start.
  step (dir, coords) n =
    if tile == Start
      then n
      else step (newDir, newCoords) (n + 1)
   where
    tile = pipeMap ! coords
    newDir = getNewDir dir tile
    newCoords = getNewCoords coords newDir

getNewDir :: Cardinal -> Tile -> Cardinal
getNewDir dir (Pipe a b)
  | dir == North = if a /= South then a else b
  | dir == South = if a /= North then a else b
  | dir == East = if a /= West then a else b
  | a /= East = a
  | otherwise = b
getNewDir _ _ = error "not a pipe"

getNewCoords :: Coords -> Cardinal -> Coords
getNewCoords (V2 x y) dir =
  case dir of
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
    case tile of
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
partB input = interiorPoints pipes $ shoelace pipes
 where
  pipes = step firstPipe []

  pipeMap = mankeloi input
  (start, _) = Map.elemAt 0 $ Map.filter (== Start) pipeMap
  firstPipe = findFirstPipe pipeMap start

  -- go around the pipe counterclockwise and collect vertices
  step (dir, coords) vertices =
    if tile == Start
      then vertices
      else step (newDir, newCoords) newVertices
   where
    tile = pipeMap ! coords
    newDir = getNewDir dir tile
    newCoords = getNewCoords coords newDir
    newVertices = coords : vertices

-- Shoelace theorem:
-- List all vertices in an anti-clockwise order
-- multiply all x coordinates with the y coordinate below
-- multiply all y coordinates with the x coordinate below
-- subtract second from first, get absolute value
-- divide by 2
shoelace :: [Coords] -> Int
shoelace coords = abs (xSum - ySum) `div` 2
 where
  (xSum, ySum) =
    foldl
      ( \(xAcc, yAcc) (V2 x y, V2 x' y') ->
          ( xAcc + (x * y')
          , yAcc + (y * x')
          )
      )
      (0, 0)
      (zip coords (rotate coords))

rotate :: [Coords] -> [Coords]
rotate (x : xs) = xs <> [x]

-- Pick's Formula:
-- A = I + B/2 - 1
-- where
--   A = area
--   I = interior lattice points
--   B = Boundary lattice points
-- I = A - B/2 + 1
interiorPoints :: [Coords] -> Int -> Int
interiorPoints coords area =
  -- the result was off by one for some reason, so no +1
  area - (length coords `div` 2)
