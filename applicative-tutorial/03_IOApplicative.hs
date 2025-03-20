{-
  The IO Applicative Functor in Haskell
  
  The IO applicative functor allows us to perform IO operations in parallel,
  unlike the IO monad which sequences operations. This is a key difference
  between applicative functors and monads.
-}

module IOApplicative where

import Control.Applicative
import Control.Concurrent (threadDelay)
-- Note: We're using a simplified approach for timing examples
-- to avoid package dependencies

-- The IO type is an instance of Applicative where:
-- pure x = return x
-- (<*>) = ap

-- Example: Reading multiple inputs
-- Using applicative style to read two inputs
readTwoInputs :: IO (String, String)
readTwoInputs = (,) <$> getLine <*> getLine

-- Example: Applying a pure function to IO values
-- Calculate the sum of two numbers entered by the user
sumUserInputs :: IO Int
sumUserInputs = (+) <$> (read <$> getLine) <*> (read <$> getLine)

-- Example: Parallel vs Sequential execution
-- In this example, we'll simulate IO operations that take time

-- A function that simulates a time-consuming IO operation
slowIO :: String -> IO String
slowIO label = do
  putStrLn $ "Starting " ++ label
  threadDelay (1000000)  -- Sleep for 1 second
  putStrLn $ "Finished " ++ label
  return $ "Result of " ++ label

-- Sequential execution using monadic style (>>=)
sequentialIO :: IO (String, String)
sequentialIO = do
  putStrLn "Running sequentially:"
  
  a <- slowIO "operation 1"
  b <- slowIO "operation 2"
  
  putStrLn $ "Total time: approximately 2 seconds"
  return (a, b)

-- Parallel execution using applicative style (<*>)
-- Note: In GHC, IO operations aren't actually executed in parallel
-- This is just to illustrate the conceptual difference
parallelIO :: IO (String, String)
parallelIO = do
  putStrLn "Running in 'parallel' (conceptually):"
  
  let result = (,) <$> slowIO "operation 1" <*> slowIO "operation 2"
  
  -- In reality, GHC will still execute these sequentially
  -- For true parallelism, you would need to use the 'parallel' package
  -- or explicit concurrency with forkIO
  
  output <- result
  putStrLn $ "Total time: approximately 2 seconds (would be 1 second with true parallelism)"
  return output

-- Example: Building a command-line form
data UserForm = UserForm
  { username :: String
  , email :: String
  , age :: Int
  }
  deriving Show

-- Function to prompt for input with a label
prompt :: String -> IO String
prompt label = do
  putStr $ label ++ ": "
  getLine

-- Collecting form data using applicative style
getUserForm :: IO UserForm
getUserForm = UserForm
  <$> prompt "Username"
  <*> prompt "Email"
  <*> (read <$> prompt "Age")

-- Example: Validating user input
data Validated a = Invalid [String] | Valid a
  deriving Show

instance Functor Validated where
  fmap _ (Invalid errs) = Invalid errs
  fmap f (Valid x) = Valid (f x)

instance Applicative Validated where
  pure = Valid
  
  Invalid errs1 <*> Invalid errs2 = Invalid (errs1 ++ errs2)
  Invalid errs <*> _ = Invalid errs
  _ <*> Invalid errs = Invalid errs
  Valid f <*> Valid x = Valid (f x)

-- Validation functions
validateUsername :: String -> Validated String
validateUsername name
  | length name < 3 = Invalid ["Username must be at least 3 characters"]
  | otherwise = Valid name

validateEmail :: String -> Validated String
validateEmail email
  | '@' `notElem` email = Invalid ["Email must contain @"]
  | otherwise = Valid email

validateAge :: Int -> Validated Int
validateAge age
  | age < 18 = Invalid ["Must be at least 18 years old"]
  | age > 120 = Invalid ["Age seems too high"]
  | otherwise = Valid age

-- Validate a user form
validateForm :: UserForm -> Validated UserForm
validateForm form = UserForm
  <$> validateUsername (username form)
  <*> validateEmail (email form)
  <*> validateAge (age form)

-- Process a form with validation
processForm :: IO ()
processForm = do
  form <- getUserForm
  putStrLn "\nValidating form..."
  
  case validateForm form of
    Valid validForm -> 
      putStrLn $ "Form is valid: " ++ show validForm
    
    Invalid errors -> do
      putStrLn "Form has errors:"
      mapM_ (\err -> putStrLn $ "- " ++ err) errors

main :: IO ()
main = do
  putStrLn "IO Applicative Examples:"
  
  putStrLn "\nExample 1: Reading two inputs (uncomment to run interactively)"
  -- putStrLn "Enter two lines of text:"
  -- result <- readTwoInputs
  -- putStrLn $ "You entered: " ++ show result
  
  putStrLn "\nExample 2: Sum of two numbers (uncomment to run interactively)"
  -- putStrLn "Enter two numbers to add:"
  -- sum <- sumUserInputs
  -- putStrLn $ "Sum: " ++ show sum
  
  putStrLn "\nExample 3: Sequential vs Parallel execution (uncomment to run)"
  -- Note: This will take time to execute
  -- putStrLn "\nSequential execution:"
  -- sequentialIO
  -- putStrLn "\nConceptually parallel execution:"
  -- parallelIO
  
  putStrLn "\nExample 4: Command-line form (uncomment to run interactively)"
  -- putStrLn "Please fill out the form:"
  -- processForm
  
  putStrLn "\nNote: Uncomment the interactive examples in the code to try them out."
