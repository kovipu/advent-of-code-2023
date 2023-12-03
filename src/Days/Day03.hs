module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.Char               (isDigit)
import           Data.Foldable.WithIndex
import           Data.List
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Data.Vector             (Vector)
import qualified Data.Vector             as Vec
import           Linear.V2               (V2 (..))
import qualified Util.Util               as U

import           Data.Attoparsec.Text
import           Data.Text               (Text)
import qualified Data.Text               as T
import           Data.Void
import qualified Program.RunDay          as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (takeWhile1 (/= '\n')) `sepBy` endOfLine

-- not actually Parsers.
mankeloi :: [Text] -> Schems
mankeloi input =
  ifoldl
    (\y acc line -> (readLine y line) <> acc)
    []
    input

readLine :: Int -> Text -> Schems
readLine y line =
  snd $ ifoldl (\x (prev, acc) char ->
    if isDigit char
      then case prev of
        -- start a new integer
        Symbol _ _ ->
          let value = read [char]
              start = V2 x y
              part = Part value (start, start)
           in (part, part : acc)
        -- if prev was an int -> append to that decimal int
        Part n (start, _end) ->
          let
            value = 10 * n + (read [char])
            part = Part value (start, V2 x y)
           in (part, part : (drop 1 acc))
    else
      let
        coords = V2 x y
        part = Symbol char coords
       in if char == '.'
            then (part, acc)
            else (part, part : acc)
  )
  (Symbol '.' (V2 0 0), [])
  (T.unpack line)


------------ TYPES ------------
type Input = [Text]

data Schem
  = Part Int (V2 Int, V2 Int)
  | Symbol Char (V2 Int)
  deriving Show

type Schems = [Schem]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  let schems = mankeloi input
   in foldl
      (isPartNumber schems)
      0
      schems

isPartNumber :: Schems -> Int -> Schem -> Int
isPartNumber schems acc schem =
  case schem of
    Symbol _ _ -> acc
    Part n (start, end) ->
      -- search around start & end for symbols
      if search schems start || search schems end
         then acc + n
         else acc

search :: Schems -> V2 Int -> Bool
search schems coords =
  isJust $ find
    (\schem ->
      case schem of
        Part _ _              -> False
        Symbol _ symbolCoords -> isAdjacent coords symbolCoords
    )
    schems

isAdjacent :: V2 Int -> V2 Int -> Bool
isAdjacent (V2 x1 y1) (V2 x2 y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let schems = mankeloi input
    in foldl
      (\acc schem ->
        case schem of
          Symbol '*' coords -> acc + getGearRatio coords schems
          _                 -> acc
      )
      0
      schems

getGearRatio :: V2 Int -> Schems -> Int
getGearRatio coords schems =
  -- find adjacent numbers
  let
    adjacentParts = filter
      (\schem ->
        case schem of
          Symbol _ _ -> False
          Part _ (start, end) -> isAdjacent coords start || isAdjacent coords end
      )
      schems
  in case adjacentParts of
    [Part a _, Part b _] -> a * b
    _                    -> 0
