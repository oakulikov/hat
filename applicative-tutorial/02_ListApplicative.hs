{-
  The List Applicative Functor in Haskell
  
  The List applicative functor represents non-deterministic computations
  with multiple possible results. It's similar to the List monad but with
  different semantics.
-}

module ListApplicative where

import Control.Applicative

-- In Haskell, lists are already defined as an applicative functor where:
-- pure x = [x]
-- fs <*> xs = [f x | f <- fs, x <- xs]

-- Example: Applying multiple functions to multiple values
-- This computes all possible combinations
applyFunctionsToValues :: [a -> b] -> [a] -> [b]
applyFunctionsToValues fs xs = fs <*> xs

-- Example usage
test1 :: [Int]
test1 = [(+1), (*2), (^2)] <*> [1, 2, 3]
-- Result: [2,3,4,2,4,6,1,4,9]
-- Explanation:
-- (+1) applied to [1,2,3] gives [2,3,4]
-- (*2) applied to [1,2,3] gives [2,4,6]
-- (^2) applied to [1,2,3] gives [1,4,9]
-- All results are concatenated

-- Applicative style with multiple arguments
-- Let's create all possible combinations of elements from multiple lists
combinations :: [a] -> [b] -> [c] -> [(a, b, c)]
combinations xs ys zs = (,,) <$> xs <*> ys <*> zs
-- This is equivalent to:
-- [(x, y, z) | x <- xs, y <- ys, z <- zs]

-- Example: Creating a deck of cards
data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum)

data Card = Card Rank Suit
  deriving (Show, Eq)

-- Create a full deck of cards using applicative style
deck :: [Card]
deck = Card <$> [Two .. Ace] <*> [Hearts, Diamonds, Clubs, Spades]

-- Example: Validating multiple conditions
-- We want to check if a number satisfies multiple predicates
allTrue :: [a -> Bool] -> a -> Bool
allTrue predicates x = and (($x) <$> predicates)

-- Example usage
isValid :: Int -> Bool
isValid = allTrue [
    (>0),           -- positive
    (<100),         -- less than 100
    even,           -- even
    (\x -> x `mod` 5 == 0)  -- divisible by 5
  ]

-- Example: Parsing with applicative functors
-- A simple parser type
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Make Parser an instance of Functor
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    return (f x, rest)

-- Make Parser an instance of Applicative
instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  
  (Parser pf) <*> (Parser px) = Parser $ \input -> do
    (f, rest1) <- pf input
    (x, rest2) <- px rest1
    return (f x, rest2)

-- Basic parsers
charP :: Char -> Parser Char
charP c = Parser $ \input -> case input of
  (x:xs) | x == c -> Just (c, xs)
  _ -> Nothing

stringP :: String -> Parser String
stringP str = sequenceA (map charP str)

-- Parsing a simple date format (YYYY-MM-DD)
data Date = Date Int Int Int
  deriving Show

digitToInt :: Char -> Int
digitToInt c = fromEnum c - fromEnum '0'

-- Parse a fixed number of digits and convert to Int
digitsP :: Int -> Parser Int
digitsP n = foldl (\acc d -> acc * 10 + d) 0 <$> sequenceA (replicate n digitP)
  where
    digitP = digitToInt <$> Parser (\input -> case input of
      (x:xs) | x >= '0' && x <= '9' -> Just (x, xs)
      _ -> Nothing)

dateP :: Parser Date
dateP = Date <$> digitsP 4 <*> (charP '-' *> digitsP 2) <*> (charP '-' *> digitsP 2)

-- Parse a date string
parseDate :: String -> Maybe Date
parseDate input = case runParser dateP input of
  Just (date, "") -> Just date  -- Ensure we consumed all input
  _ -> Nothing

main :: IO ()
main = do
  putStrLn "List Applicative Examples:"
  
  putStrLn "\nApplying multiple functions to multiple values:"
  print $ [(+1), (*2), (^2)] <*> [1, 2, 3]
  
  putStrLn "\nAll combinations of [1,2] [3,4] [5,6]:"
  print $ combinations [1,2] [3,4] [5,6]
  
  putStrLn "\nFirst 5 cards from a deck:"
  print $ take 5 deck
  
  putStrLn "\nValidating numbers with multiple conditions:"
  putStrLn $ "Is 20 valid? " ++ show (isValid 20)  -- True (positive, <100, even, divisible by 5)
  putStrLn $ "Is 25 valid? " ++ show (isValid 25)  -- False (not even)
  putStrLn $ "Is 30 valid? " ++ show (isValid 30)  -- True
  
  putStrLn "\nParsing dates with applicative parser:"
  putStrLn $ "Parse '2023-04-15': " ++ show (parseDate "2023-04-15")
  putStrLn $ "Parse '2023-4-15': " ++ show (parseDate "2023-4-15")  -- Fails (month needs 2 digits)
