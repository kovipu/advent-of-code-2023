module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.Char
import           Data.Foldable.WithIndex
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Internal.Search as TS
import           Data.Vector               (Vector)
import qualified Data.Vector               as Vec
import qualified Util.Util                 as U

import           Data.Attoparsec.Text
import           Data.Void
import qualified Program.RunDay            as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (takeWhile1 (/= '\n')) `sepBy` endOfLine

------------ TYPES ------------
type Input = [Text]

type OutputA = Maybe Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input = sum <$> traverse matchLine input

matchLine :: Text -> Maybe Int
matchLine line = do
  first <- T.find isDigit line
  last <- T.find isDigit $ T.reverse line
  pure $ read [first, last]

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  sum $ map matchLine' input

matchLine' :: Text -> Int
matchLine' line =
  let
    (firstIdx, first) = findBy minimum (<) 9999999
    (lastIdx, last) = findBy maximum (>) (-1)

    findBy f c st =
      ifoldl
        (\idx acc@(prevFirst, _) (a, b) ->
          let
            match = TS.indices a line <> TS.indices b line
          in
          if not (null match) && f match `c` prevFirst
             then (f match, idx)
             else acc
        )
        (st, -1)
        nums
  in
  (first * 10) + last

nums :: [(Text, Text)]
nums = [ ("zero", "0"), ("one", "1"), ("two", "2"), ("three", "3"), ("four", "4"), ("five", "5"), ("six", "6"), ("seven", "7"), ("eight", "8"), ("nine", "9") ]
