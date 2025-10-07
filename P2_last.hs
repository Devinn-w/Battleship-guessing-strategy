-- Proj2.hs
-- Author: BINZHEN WEI(StudentID: 1575618)




-- Summay of file purpose
-- This file implements a ship guessing strategy 
--                                   for a Battleship-style grid game.




-- File-level documentation
-- This file implements a logical ship-search guessing game 
--                     on a 4×8 grid using functional programming in Haskell.
--
-- The goal is to guess the exact locations of three hidden ships
--                                      using feedback from previous guesses.
-- The main functions include:
--   • `toLocation` / `fromLocation`: converting between coordinates 
--                                                       and string format.
--
--   • `feedback`: calculating how close a guess is to the target.
--
--   • `initialGuess`: generating the first guess 
--                                          and initializing the game state.
--
--   • `nextGuess`: refining guesses based on received feedback 
--                                   and eliminating inconsistent candidates.
--
-- The game state is updated functionally without mutable variables, 
--  and the algorithm selects guesses based on minimizing the expected number
--   of remaining candidates to optimize performance.











--------------------------------------------------------
-- Module Declaration and Imports
--------------------------------------------------------

-- Export the main interface for the Ship Search game logic
module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

-- List and utility imports for filtering, grouping and list operations
import Data.List (group, sort, (\\), elemIndex)

-- Used for handling Maybe values like fromJust/fromMaybe
import Data.Maybe (fromJust, fromMaybe)

-- Used to normalize input like 'a1' to 'A1'
import Data.Char (toUpper)

--------------------------------------------------------
-- Data Type Definitions
--------------------------------------------------------

-- | A grid column, from A to H.
data Column = A | B | C | D | E | F | G | H
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A grid row, from 1 to 4.
data Row = One | Two | Three | Four
  deriving (Eq, Ord, Enum, Bounded)

instance Show Row where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"

-- | Represents a single board location on a 4x8 grid.
-- Example: Location A One corresponds to "A1"
data Location = Location Column Row
  deriving (Eq, Ord)

instance Show Location where
  show (Location c r) = show c ++ show r

--------------------------------------------------------
-- Type Aliases
--------------------------------------------------------

-- Type Aliases
-- | A guess is a list of three distinct board locations.
-- | A GameState holds all possible valid target combinations still consistent
-- with previous feedback.
type Guess = [Location]
type GameState = [Guess]

--------------------------------------------------------
-- Location Conversion Functions
--------------------------------------------------------


-- | Converts a two-character string (e.g., "A1") to a Location.
-- Returns Nothing if the string is invalid.
toLocation :: String -> Maybe Location
toLocation [c, r]
  | columnChar `elem` ['A'..'H'] && r `elem` ['1'..'4'] =
      Just $ Location (toEnum (fromEnum columnChar - fromEnum 'A')) 
                                                    (toEnum (read [r] - 1))
  | otherwise = Nothing
  where
    columnChar = toUpper c
toLocation _ = Nothing

-- | Converts a Location back to its two-character string form.
-- Should satisfy: toLocation (fromLocation loc) == Just loc
fromLocation :: Location -> String
fromLocation = show

--------------------------------------------------------
-- Precomputed Data Sets
--------------------------------------------------------

-- | All 32 possible grid locations.
allLocations :: [Location]
allLocations = [Location c r | c <- [A .. H], r <- [One .. Four]]


-- | All sorted combinations of 3 distinct locations (total 4960).
allGameState :: [Guess]
allGameState = [sort [a, b, c] | a <- allLocations, 
                                  b <- allLocations, 
                                   c <- allLocations, a < b, b < c]


--------------------------------------------------------
-- Feedback Logic
--------------------------------------------------------

-- | Computes the distance between two Locations using Chebyshev distance.
-- Distance 0: exact match, 1: adjacent (including diagonals), 2: 2-away, etc.
locationDistance :: Location -> Location -> Int
locationDistance (Location c1 r1) (Location c2 r2) =
  max (distance c1 c2) (distance r1 r2)


distance :: Enum a => a -> a -> Int
distance x y = abs (fromEnum x - fromEnum y)


-- | Calculates feedback from comparing a guess to a target.
-- Each guess location is scored only by its closest target ship.
feedback :: Guess -> Guess -> (Int, Int, Int)
feedback target guess = (count 0, count 1, count 2)
  where
    dists = [minimum [locationDistance g t | t <- target] | g <- guess]
    count n = length (filter (== n) dists)

--------------------------------------------------------
-- Guessing Strategy
--------------------------------------------------------

-- | Provides an initial guess and full game state 
--                                              (all 3-location combinations).
-- The guess is hardcoded for optimal information gain.
initialGuess :: (Guess, GameState)
initialGuess = (initGuess, allGameState \\ [initGuess])
  where
    initGuess = sort $ map (fromJust . toLocation) ["A1", "H1", "A3"]


-- | Generates the next guess based on the last guess and feedback.
-- Filters the GameState 
--             to only include candidates consistent with all feedback so far.
-- Then selects the guess 
--                   that minimizes the expected size of remaining candidates.
nextGuess :: (Guess, GameState) -> (Int, Int, Int) -> (Guess, GameState)
nextGuess (lastGuess, lastGameState) score
  | null candidates = error $ "\n No valid candidates remain for score " 
            ++ show score 
                  ++ " after guess " 
                       ++ show (map fromLocation lastGuess)
  | otherwise       = (newGuess, candidates \\ [newGuess])
  where
    sortedGuess = sort lastGuess
    restGameState = lastGameState \\ [sortedGuess]
    candidates = [g | g <- restGameState, feedback g sortedGuess == score]
    newGuess = lowestExpected candidates


-- | Computes the expected number of remaining candidates 
--                                                  if a given guess is made.
expectedValue :: Guess -> GameState -> Double
expectedValue guess state = sum [n * n / total | n <- numList]
  where
    stateScores = map (feedback guess) state
    grouped = group $ sort stateScores
    total = fromIntegral $ length stateScores
    numList = map (fromIntegral . length) grouped


-- | From a GameState, chooses the guess that yields the lowest expected value.
-- If multiple guesses have the same expected value, selects the first.
lowestExpected :: GameState -> Guess
lowestExpected [] = error " lowestExpected received empty GameState"
lowestExpected state =
  case elemIndex minVal expectedList of
    Just idx -> state !! idx
    Nothing  -> head state
  where
    expectedList = map (`expectedValue` state) state
    minVal = minimum expectedList