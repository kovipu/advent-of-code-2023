module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.List
import qualified Data.List            as List
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (Maybe (..), listToMaybe, mapMaybe)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import qualified Util.Util            as U

import           Control.Applicative  (Alternative (some))
import           Data.Attoparsec.Text
import           Data.Void
import           Debug.Trace          (trace, traceShow)
import qualified Program.RunDay       as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy` endOfLine

parseLine = do
  chars <- many1 (notChar ' ')
  space
  nums <- decimal `sepBy1` char ','
  pure (chars, nums)

------------ TYPES ------------
type Input = [Line]

type Line = (String, [Int])

type OutputA = Int

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum $ map (\(str, nums) -> length $ filter (matchesRule nums) $ createCombinations str) input

createCombinations :: String -> [String]
createCombinations str =
  -- recurse until string ends -> return tails
  -- switch the 1st question mark into both # and .
  -- if no question marks left -> stop recursion
  if '?' `notElem` str
    then [str]
    else concatMap createCombinations [replace str '#', replace str '.']
 where
  -- replace first ? with c
  replace :: String -> Char -> String
  replace ('?' : xs) c = c : xs
  replace (x : xs) c   = x : replace xs c
  replace "" _         = ""

matchesRule :: [Int] -> String -> Bool
matchesRule [] str = '#' `notElem` str
matchesRule (n : ns) str =
  -- str must have nums of #'s in a row
  -- remove all prefixing .
  -- is head exactly n #'s?
  -- if not -> False
  -- if yes -> recurse
  (length str' >= n && all (== '#') (List.take n str') && nth str' n /= Just '#') && matchesRule ns (drop (n + 1) str')
 where
  str' = dropWhile (== '.') str

  nth l n =
    if n >= length l then Nothing else Just $ l !! n

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
