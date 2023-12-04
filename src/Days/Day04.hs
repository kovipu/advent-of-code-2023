module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.List            ()
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           ()
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import qualified Util.Util            as U

import           Control.Applicative  (Alternative (many), optional)
import           Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy,
                                       space, string)
import           Data.Map             ((!))
import qualified Program.RunDay       as R (Day, runDay)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseLine `sepBy` endOfLine

parseLine :: Parser Card
parseLine = do
  string "Card"
  many space
  cardId <- decimal
  char ':'
  many space
  winningNumbers <- decimal `sepBy` many space
  many space
  char '|'
  many space
  myNumbers <- decimal `sepBy` many space
  pure Card{cardId, winningNumbers, myNumbers}

------------ TYPES ------------
type Input = [Card]

data Card = Card {cardId :: Int, winningNumbers :: [Int], myNumbers :: [Int]}
  deriving (Show)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA cards = sum $ map findPoints cards

findPoints :: Card -> Int
findPoints card =
  let n = length $ wins card
   in case n of
        0 -> 0
        _ -> 2 ^ (n - 1)

wins :: Card -> [Int]
wins Card{winningNumbers, myNumbers} = filter (`elem` winningNumbers) myNumbers

------------ PART B ------------
partB :: Input -> Int
partB cards =
  let cardMap = Map.fromList $ map (\card@Card{cardId} -> (cardId, card)) cards
   in foldl
        ( \acc card ->
            acc + wonCards cardMap card
        )
        0
        cards

wonCards :: Map Int Card -> Card -> Int
wonCards cardMap card@Card{cardId} =
  let numWins = length $ wins card
      newCards = if numWins > 0 then [(cardId + 1) .. (cardId + numWins)] else []
   in foldl
        ( \acc cid ->
            acc + wonCards cardMap (cardMap ! cid)
        )
        1
        newCards
