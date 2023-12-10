module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import           Control.Applicative  (Alternative (many), (<|>))
import           Data.Attoparsec.Text as AT (Parser, char, endOfLine, sepBy1,
                                             space, string, take)
import           Data.Functor         (($>))
import           Data.Map.Strict      (Map, (!))
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (fromJust)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import           Data.Void            (Void)
import           Debug.Trace          (trace)
import qualified Program.RunDay       as R (Day, runDay)
import qualified Util.Util            as U
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  directions <- many parseDir
  many space
  nodes <- parseNode `sepBy1` endOfLine
  let nodeMap = Map.fromList nodes
  pure Input{directions, nodeMap}

parseDir = char 'R' $> R <|> char 'L' $> L

parseNode = do
  key <- AT.take 3
  string " = ("
  l <- AT.take 3
  string ", "
  r <- AT.take 3
  char ')'
  pure (key, (l, r))

------------ TYPES ------------
data Input = Input {directions :: [Direction], nodeMap :: NodeMap}
  deriving (Show)

data Direction = L | R
  deriving (Show, Eq)

type NodeMap = Map Text (Text, Text)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = findZZZ "AAA" 0

findZZZ :: Text -> Int -> Input -> Int
findZZZ curr n input@Input{directions, nodeMap} =
  if curr == "ZZZ"
    then n
    else findZZZ new (n + 1) input
 where
  (l, r) = nodeMap ! curr
  step = directions !! (n `mod` length directions)
  new = if step == L then l else r

------------ PART B ------------
partB :: Input -> OutputB
partB input@Input{directions, nodeMap} =
  lcmm $ map (findEnd input 0) starts
 where
  starts = map fst $ Map.toList $ Map.filterWithKey (\pos _ -> T.takeEnd 1 pos == "A") nodeMap

findEnd :: Input -> Int -> Text -> Int
findEnd input@Input{directions, nodeMap} n curr =
  if T.takeEnd 1 curr == "Z"
    then n
    else findEnd input (n + 1) new
 where
  step = directions !! (n `mod` length directions)
  (l, r) = nodeMap ! curr
  new = if step == L then l else r

-- least common multiple of xs
lcmm = foldr lcm 1
