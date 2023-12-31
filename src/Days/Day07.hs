module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import           Control.Applicative     (Alternative (many), (<|>))
import           Control.Monad.Extra     (liftMaybe)
import           Data.Attoparsec.Text
import           Data.FixedList          (FixedList5, fromFoldable)
import           Data.Foldable           (toList)
import           Data.Foldable.WithIndex (FoldableWithIndex (ifoldl))
import           Data.Functor            (($>))
import           Data.List
import           Data.List.NonEmpty      (sortWith)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe
import           Data.Void
import           Debug.Trace             (trace)
import qualified Program.RunDay          as R (Day, runDay)
import qualified Util.Util               as U
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = parseHand `sepBy1` endOfLine

parseHand :: Parser Hand
parseHand = do
  cards' <- many parseCard
  cards <- liftMaybe $ fromFoldable cards'
  space
  bid <- decimal
  pure Hand{cards, bid}

parseCard :: Parser Card
parseCard =
  char 'A' $> Ace <|> char 'K' $> King <|> char 'Q' $> Queen <|> char 'J' $> Joker <|> char 'T' $> Ten <|> char '9' $> Nine <|> char '8' $> Eight <|> char '7' $> Seven <|> char '6' $> Six <|> char '5' $> Five <|> char '4' $> Four <|> char '3' $> Three <|> char '2' $> Two

------------ TYPES ------------
type Input = [Hand]

data Hand = Hand {cards :: FixedList5 Card, bid :: Int}
  deriving (Show)

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Queen | King | Ace
  deriving (Show, Eq, Ord)

data HandType = High | OnePair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind
  deriving (Show, Eq, Ord)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
  ifoldl
    (\idx acc Hand{bid} -> (idx + 1) * bid + acc)
    0
    $ sortBy (compareHands getHandType) input

compareHands :: (FixedList5 Card -> HandType) -> Hand -> Hand -> Ordering
compareHands typeFn Hand{cards = a} Hand{cards = b} =
  let
    at = typeFn a
    bt = typeFn b
   in
    if at /= bt
      then compare at bt
      else findHigher (toList a) (toList b)

getHandType :: FixedList5 Card -> HandType
getHandType cards
  | findNCards 5 = FiveKind
  | findNCards 4 = FourKind
  | findNCards 3 && findNCards 2 = FullHouse
  | findNCards 3 = ThreeKind
  | length (filter (\(_c, n) -> n == 2) frequencies) == 2 = TwoPair
  | findNCards 2 = OnePair
  | otherwise = High
 where
  frequencies =
    Map.toList
      $ foldl (\acc c -> Map.insertWith (+) c 1 acc) Map.empty cards
  findNCards num = isJust $ find (\(_c, n) -> num == n) frequencies

findHigher (a : as) (b : bs)
  | a /= b = compare a b
  | not $ null as = findHigher as bs
  | otherwise = EQ

------------ PART B ------------
partB :: Input -> OutputB
partB input =
  ifoldl
    (\idx acc Hand{bid} -> (idx + 1) * bid + acc)
    0
    $ sortBy (compareHands getHandType') input

getHandType' :: FixedList5 Card -> HandType
getHandType' cards
  | findNCards 5 || numJokers == 5 = FiveKind
  | findNCards 4 = FourKind
  | findNCards 3 && findNCards 2 = FullHouse
  | findNCards 3 = ThreeKind
  | length (filter (\(_c, n) -> n == 2) frequencies') == 2 = TwoPair
  | findNCards 2 = OnePair
  | otherwise = High
 where
  frequencies = foldl (\acc c -> Map.insertWith (+) c 1 acc) Map.empty cards
  -- move number of jokers into the card with most occurences.
  numJokers = fromMaybe 0 $ Map.lookup Joker frequencies
  frequencies' = Map.toList $ Map.delete Joker frequencies
  (topCard, _) = last $ sortOn snd frequencies'
  withJokers =
    map
      ( \(c, n) ->
          if c == topCard then (c, n + numJokers) else (c, n)
      )
      frequencies'

  findNCards num = isJust $ find (\(_c, n) -> num == n) withJokers
