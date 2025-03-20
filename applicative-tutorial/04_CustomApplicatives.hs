{-
  Creating Custom Applicative Functors in Haskell
  
  This module demonstrates how to create your own applicative functors
  to encapsulate specific computational contexts.
-}

module CustomApplicatives where

import Control.Applicative
import Data.Monoid (Sum(..), Product(..))

-- Example 1: A simple Pair applicative functor
-- This pairs a value with some extra data
data Pair a = Pair a a
  deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

-- Example usage
pairExample :: Pair Int
pairExample = (+) <$> Pair 1 10 <*> Pair 2 20
-- Result: Pair 3 30

-- Example 2: A MyConst applicative functor
-- This ignores the value and only keeps the constant
newtype MyConst m a = MyConst { getMyConst :: m }
  deriving Show

instance Functor (MyConst m) where
  fmap _ (MyConst m) = MyConst m

instance Monoid m => Applicative (MyConst m) where
  pure _ = MyConst mempty
  MyConst m1 <*> MyConst m2 = MyConst (m1 `mappend` m2)

-- Example usage: Collecting validation errors
validatePositive :: Int -> MyConst [String] Int
validatePositive x
  | x > 0 = MyConst []
  | otherwise = MyConst ["Value must be positive"]

validateEven :: Int -> MyConst [String] Int
validateEven x
  | even x = MyConst []
  | otherwise = MyConst ["Value must be even"]

validateDivisibleBy3 :: Int -> MyConst [String] Int
validateDivisibleBy3 x
  | x `mod` 3 == 0 = MyConst []
  | otherwise = MyConst ["Value must be divisible by 3"]

-- Validate a number with multiple criteria
validateNumber :: Int -> [String]
validateNumber x = getMyConst $ validatePositive x *> validateEven x *> validateDivisibleBy3 x

-- Example 3: A ZipList applicative functor
-- This applies functions to values at the same position
newtype ZipList' a = ZipList' { getZipList' :: [a] }
  deriving Show

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (map f xs)

instance Applicative ZipList' where
  pure x = ZipList' (repeat x)
  ZipList' fs <*> ZipList' xs = ZipList' (zipWith ($) fs xs)

-- Example usage
zipExample :: ZipList' Int
zipExample = (+) <$> ZipList' [1, 2, 3] <*> ZipList' [10, 20, 30]
-- Result: ZipList' [11, 22, 33]

-- Example 4: A Reader applicative functor
-- This provides read-only access to an environment
newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader e) where
  pure x = Reader (\_ -> x)
  Reader f <*> Reader g = Reader (\e -> f e (g e))

-- Example usage: Configuration-based computation
data Config = Config {
  multiplier :: Int,
  offset :: Int
}

computeValue :: Int -> Reader Config Int
computeValue x = Reader (\config -> x * multiplier config + offset config)

combineValues :: Int -> Int -> Reader Config Int
combineValues x y = (+) <$> computeValue x <*> computeValue y

-- Example 5: A Validation applicative functor
-- Similar to Either, but accumulates errors
data Validation e a = Failure e | Success a
  deriving Show

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  
  Failure e1 <*> Failure e2 = Failure (e1 `mappend` e2)
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e
  Success f <*> Success a = Success (f a)

-- Example usage: Form validation
validateName :: String -> Validation [String] String
validateName name
  | length name >= 2 = Success name
  | otherwise = Failure ["Name must be at least 2 characters"]

validateAge :: Int -> Validation [String] Int
validateAge age
  | age >= 18 = Success age
  | otherwise = Failure ["Age must be at least 18"]

data User = User { userName :: String, userAge :: Int }
  deriving Show

validateUser :: String -> Int -> Validation [String] User
validateUser name age = User <$> validateName name <*> validateAge age

-- Example 6: An Identity applicative functor
-- This is just a simple wrapper with no additional context
newtype Identity a = Identity { runIdentity :: a }
  deriving Show

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

-- Example usage: Applying a function without any context
identityExample :: Identity Int
identityExample = (+) <$> Identity 5 <*> Identity 10
-- Result: Identity 15

main :: IO ()
main = do
  putStrLn "Custom Applicative Functors Examples:"
  
  putStrLn "\nPair Applicative:"
  print $ pairExample  -- Pair 3 30
  
  putStrLn "\nConst Applicative (Validation):"
  putStrLn $ "Validate 6: " ++ show (validateNumber 6)  -- []
  putStrLn $ "Validate 5: " ++ show (validateNumber 5)  -- ["Value must be even", "Value must be divisible by 3"]
  putStrLn $ "Validate -6: " ++ show (validateNumber (-6))  -- ["Value must be positive"]
  
  putStrLn "\nZipList Applicative:"
  print $ zipExample  -- ZipList' [11, 22, 33]
  
  putStrLn "\nReader Applicative:"
  let config = Config { multiplier = 2, offset = 10 }
  putStrLn $ "combineValues 3 4 with config: " ++ 
             show (runReader (combineValues 3 4) config)  -- (3*2+10) + (4*2+10) = 16 + 18 = 34
  
  putStrLn "\nValidation Applicative:"
  print $ validateUser "John" 20  -- Success (User {userName = "John", userAge = 20})
  print $ validateUser "J" 20  -- Failure ["Name must be at least 2 characters"]
  print $ validateUser "John" 16  -- Failure ["Age must be at least 18"]
  print $ validateUser "J" 16  -- Failure ["Name must be at least 2 characters", "Age must be at least 18"]
  
  putStrLn "\nIdentity Applicative:"
  print $ identityExample  -- Identity 15
