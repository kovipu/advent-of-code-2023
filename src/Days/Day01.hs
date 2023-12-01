module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.Char
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Data.Text            as T
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import qualified Util.Util            as U

import           Data.Attoparsec.Text
import           Data.Void
import qualified Program.RunDay       as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (takeWhile1 (/= '\n')) `sepBy` endOfLine

------------ TYPES ------------
type Input = [Text]

type OutputA = Maybe Int

type OutputB = Void

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
partB = error "Not implemented yet!"
