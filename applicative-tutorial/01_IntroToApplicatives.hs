{-
  Introduction to Applicative Functors in Haskell
  
  Applicative functors are a powerful abstraction that sits between functors and monads
  in the Haskell type class hierarchy. They provide a way to apply functions wrapped in
  a context to values wrapped in the same context.
-}

module IntroToApplicatives where

import Control.Applicative (liftA2)

-- The Applicative typeclass definition (simplified):
{-
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
-}

-- Key operations:
-- 1. pure: Wraps a value in the applicative context
-- 2. (<*>): Applies a function in a context to a value in a context

-- Let's start with a simple example: the Maybe applicative

-- Example: Combining two Maybe values
-- We want to add two Maybe Int values
addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes mx my = 
  case mx of
    Nothing -> Nothing
    Just x -> case my of
      Nothing -> Nothing
      Just y -> Just (x + y)

-- The same operation using Applicative
addMaybesApplicative :: Maybe Int -> Maybe Int -> Maybe Int
addMaybesApplicative mx my = pure (+) <*> mx <*> my
-- This can be read as: "Apply the function (+) to the values inside mx and my,
-- if they exist"

-- We can also use the liftA2 helper function
addMaybesLiftA2 :: Maybe Int -> Maybe Int -> Maybe Int
addMaybesLiftA2 mx my = liftA2 (+) mx my

-- Let's test these functions
test1 :: Maybe Int
test1 = addMaybesApplicative (Just 3) (Just 5)  -- Just 8

test2 :: Maybe Int
test2 = addMaybesApplicative (Just 3) Nothing   -- Nothing

test3 :: Maybe Int
test3 = addMaybesApplicative Nothing (Just 5)   -- Nothing

-- The key insight: Applicative functors allow us to apply functions to values
-- inside contexts, handling the context automatically.

-- Applicative style with multiple arguments
-- Let's create a Person data type
data Person = Person { name :: String, age :: Int, address :: String }
  deriving Show

-- Creating a Person from potentially missing data
makePerson :: Maybe String -> Maybe Int -> Maybe String -> Maybe Person
makePerson mname mage maddress = 
  pure Person <*> mname <*> mage <*> maddress

-- This is much cleaner than nested case expressions!

-- The Applicative Laws (these must be satisfied by all applicative functors):
-- 1. Identity:      pure id <*> v = v
-- 2. Composition:   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 3. Homomorphism:  pure f <*> pure x = pure (f x)
-- 4. Interchange:   u <*> pure y = pure ($ y) <*> u

-- Applicative vs Functor
-- Functor (fmap or <$>) lets us apply a function to a value in a context:
-- fmap :: (a -> b) -> f a -> f b

-- Applicative (<*>) lets us apply a function in a context to a value in a context:
-- (<*>) :: f (a -> b) -> f a -> f b

-- This is more powerful because the function itself can be in a context

-- Convenient operator: <$>
-- (<$>) is just fmap with the arguments flipped
-- This lets us write:
-- Person <$> mname <*> mage <*> maddress
-- Instead of:
-- pure Person <*> mname <*> mage <*> maddress

makePersonAlt :: Maybe String -> Maybe Int -> Maybe String -> Maybe Person
makePersonAlt mname mage maddress = 
  Person <$> mname <*> mage <*> maddress

main :: IO ()
main = do
  putStrLn "Testing Applicative Functors:"
  putStrLn $ "addMaybesApplicative (Just 3) (Just 5) = " ++ show test1
  putStrLn $ "addMaybesApplicative (Just 3) Nothing = " ++ show test2
  putStrLn $ "addMaybesApplicative Nothing (Just 5) = " ++ show test3
  
  let validPerson = makePerson (Just "John") (Just 30) (Just "123 Main St")
  putStrLn $ "Valid Person: " ++ show validPerson
  
  let invalidPerson = makePerson (Just "Jane") Nothing (Just "456 Oak Ave")
  putStrLn $ "Invalid Person (missing age): " ++ show invalidPerson
