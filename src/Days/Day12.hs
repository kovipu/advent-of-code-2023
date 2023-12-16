module Days.Day12 (runDay) where

{- ORMOLU_DISABLE -}
import           Data.List
import qualified Data.List            as List
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Maybe           (Maybe (..))
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Vector          (Vector)
import qualified Data.Vector          as Vec
import qualified Util.Util            as U

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
type Input = [([Char], [Int])]

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

-- where
--  (chars, nums) = input !! 5

-- countArrangements :: [Char] -> [] [Char] -> [Int] -> ([[Char]], Int)
-- countArrangements start handled chars nums =
--   foldl
--     ( \(handled', acc) t ->
--         let (h, n) = numValidArrangements start handled' t nums
--          in (h, acc + n)
--     )
--     (handled, 0)
--     borkedTails
--  where
--   -- tails match multiple times because ofcourse they do
--   -- how to filter out dupes?
--   -- 1) remove tails that start with multiple .
--   -- 2) remove tails that have been handled

--   borkedTails = filter (\t -> safeHead t /= Just '.') $ tails chars

--   numValidArrangements handled' chars' nums' =
--     case nums' of
--       [] -> (handled', 1)
--       (n : ns) ->
--         -- each tail of chars could match
--         if headMatches chars' n
--           then countArrangements (start <> List.take n chars') (drop (n + 1) chars') ns
--           else (handled', 0)

--   headMatches chars' n =
--     -- are first n characters either # or ?
--     -- and the n+1-th a . or ? or empty
--     length chars' >= n && notElem '.' (List.take n chars') && nth chars' n /= Just '#'

--   safeHead l = case l of
--     [] -> Nothing
--     _  -> Just $ head l

--   nth l n =
--     if n >= length l then Nothing else Just $ l !! n

-- lg v = trace v v

-- isValidArrangement :: [Char] -> Int -> Bool
-- isValidArrangement chars n =
--   length a == n && all (\c -> c == '?' || c == '#') a && e /= Just '#'
--  where
--   a = List.take n chars
--   e = chars `nth` n

-- if tail startsWith '#/?' * num + ' /?'
-- -> recurse with those removed and the number removed

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
