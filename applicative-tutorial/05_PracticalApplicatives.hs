{-
  Practical Applications of Applicative Functors in Haskell
  
  This module demonstrates how applicative functors are used to solve
  real-world programming problems in a clean and composable way.
-}

module PracticalApplicatives where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.List (intercalate, lookup)
-- Using association lists instead of Data.Map to avoid package dependencies

-- Example 1: Configuration handling
-- Imagine we have a configuration with optional values

data ServerConfig = ServerConfig {
  serverHost :: String,
  serverPort :: Int,
  serverUser :: String,
  serverPassword :: Maybe String,
  serverTimeout :: Int
}

-- Default configuration
defaultConfig :: ServerConfig
defaultConfig = ServerConfig {
  serverHost = "localhost",
  serverPort = 8080,
  serverUser = "admin",
  serverPassword = Nothing,
  serverTimeout = 30
}

-- Parse configuration from a list of key-value pairs
parseConfig :: [(String, String)] -> ServerConfig
parseConfig configList = ServerConfig {
  serverHost = fromMaybe (serverHost defaultConfig) (lookup "host" configList),
  serverPort = fromMaybe (serverPort defaultConfig) (readMaybe =<< lookup "port" configList),
  serverUser = fromMaybe (serverUser defaultConfig) (lookup "user" configList),
  serverPassword = lookup "password" configList,
  serverTimeout = fromMaybe (serverTimeout defaultConfig) (readMaybe =<< lookup "timeout" configList)
}
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- Using applicative style for cleaner configuration parsing
parseConfigApplicative :: [(String, String)] -> ServerConfig
parseConfigApplicative configList = 
  ServerConfig
    <$> lookupOr "host" (serverHost defaultConfig)
    <*> lookupOrRead "port" (serverPort defaultConfig)
    <*> lookupOr "user" (serverUser defaultConfig)
    <*> pure (lookup "password" configList)
    <*> lookupOrRead "timeout" (serverTimeout defaultConfig)
    $ configList
  where
    lookupOr :: String -> String -> [(String, String)] -> String
    lookupOr key def list = fromMaybe def (lookup key list)
    
    lookupOrRead :: Read a => String -> a -> [(String, String)] -> a
    lookupOrRead key def list = fromMaybe def (readMaybe =<< lookup key list)
    
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- Example 2: Form validation
-- A more realistic form validation example

data UserProfile = UserProfile {
  profileName :: String,
  profileEmail :: String,
  profileAge :: Int,
  profileWebsite :: Maybe String
}
  deriving Show

-- Validation result type
data ValidationResult a = 
    ValidationSuccess a
  | ValidationFailure [String]
  deriving Show

instance Functor ValidationResult where
  fmap f (ValidationSuccess a) = ValidationSuccess (f a)
  fmap _ (ValidationFailure errs) = ValidationFailure errs

instance Applicative ValidationResult where
  pure = ValidationSuccess
  
  ValidationFailure errs1 <*> ValidationFailure errs2 = ValidationFailure (errs1 ++ errs2)
  ValidationFailure errs <*> _ = ValidationFailure errs
  _ <*> ValidationFailure errs = ValidationFailure errs
  ValidationSuccess f <*> ValidationSuccess a = ValidationSuccess (f a)

-- Validation functions
validateName :: String -> ValidationResult String
validateName name
  | length name < 2 = ValidationFailure ["Name must be at least 2 characters"]
  | length name > 50 = ValidationFailure ["Name must be at most 50 characters"]
  | otherwise = ValidationSuccess name

validateEmail :: String -> ValidationResult String
validateEmail email
  | '@' `notElem` email = ValidationFailure ["Email must contain @"]
  | length email < 5 = ValidationFailure ["Email is too short"]
  | otherwise = ValidationSuccess email

validateAge :: Int -> ValidationResult Int
validateAge age
  | age < 13 = ValidationFailure ["Must be at least 13 years old"]
  | age > 120 = ValidationFailure ["Age seems too high"]
  | otherwise = ValidationSuccess age

validateWebsite :: Maybe String -> ValidationResult (Maybe String)
validateWebsite Nothing = ValidationSuccess Nothing
validateWebsite (Just url)
  | "http" `isPrefixOf` url = ValidationSuccess (Just url)
  | otherwise = ValidationFailure ["Website must start with http:// or https://"]
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- Validate a user profile
validateUserProfile :: String -> String -> Int -> Maybe String -> ValidationResult UserProfile
validateUserProfile name email age website = 
  UserProfile 
    <$> validateName name
    <*> validateEmail email
    <*> validateAge age
    <*> validateWebsite website

-- Example 3: Parsing command-line arguments
-- A simple command-line argument parser

data Command = 
    Add String Int
  | Remove String
  | List
  | Help
  deriving Show

-- Parse command-line arguments
parseArgs :: [String] -> Maybe Command
parseArgs args = case args of
  ["add", name, valueStr] -> Add name <$> readMaybe valueStr
  ["remove", name] -> Just (Remove name)
  ["list"] -> Just List
  ["help"] -> Just Help
  _ -> Nothing
  where
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

-- Example 4: Building a query string
-- Construct a URL query string from parameters

data QueryParam = 
    RequiredParam String String
  | OptionalParam String (Maybe String)
  deriving Show

buildQueryString :: [QueryParam] -> String
buildQueryString params = 
  "?" ++ intercalate "&" (map paramToString $ filter isValidParam params)
  where
    paramToString (RequiredParam key value) = key ++ "=" ++ value
    paramToString (OptionalParam key (Just value)) = key ++ "=" ++ value
    paramToString _ = ""
    
    isValidParam (RequiredParam _ _) = True
    isValidParam (OptionalParam _ (Just _)) = True
    isValidParam _ = False

-- Using applicative style to build a query
buildQuery :: String -> Int -> Maybe String -> String -> String
buildQuery term limit mfilter sort =
  buildQueryString
    [ RequiredParam "q" term
    , RequiredParam "limit" (show limit)
    , OptionalParam "filter" mfilter
    , RequiredParam "sort" sort
    ]

-- Example 5: Combining computations with different effects
-- Using liftA2, liftA3, etc. to combine computations

-- Combine two Maybe values
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing
safeDivide x y = Just (x `div` y)

-- Combine three Maybe values
computeAverage :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Double
computeAverage mx my mz = 
  (\x y z -> fromIntegral (x + y + z) / 3.0) <$> mx <*> my <*> mz

-- Or using liftA3
computeAverageAlt :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Double
computeAverageAlt mx my mz = 
  liftA3 (\x y z -> fromIntegral (x + y + z) / 3.0) mx my mz

-- Example 6: Parallel computation (conceptually)
-- Applicative functors allow for "parallel" computation

data Computation a = Computation String a
  deriving Show

instance Functor Computation where
  fmap f (Computation label x) = Computation label (f x)

instance Applicative Computation where
  pure x = Computation "pure" x
  Computation label1 f <*> Computation label2 x = 
    Computation (label1 ++ " <*> " ++ label2) (f x)

instance Monad Computation where
  return = pure
  Computation label x >>= f = 
    let Computation label' y = f x
    in Computation (label ++ " >>= " ++ label') y

-- Example computations
comp1 :: Computation Int
comp1 = Computation "comp1" 5

comp2 :: Computation Int
comp2 = Computation "comp2" 7

-- Sequential computation (monadic style)
sequentialComp :: Computation Int
sequentialComp = do
  x <- comp1
  y <- comp2
  return (x + y)

-- Parallel computation (applicative style)
parallelComp :: Computation Int
parallelComp = (+) <$> comp1 <*> comp2

main :: IO ()
main = do
  putStrLn "Practical Applicative Functor Examples:"
  
  putStrLn "\nExample 1: Configuration Handling"
  let configList = [("host", "example.com"), ("port", "9000")]
  let config = parseConfigApplicative configList
  putStrLn $ "Host: " ++ serverHost config
  putStrLn $ "Port: " ++ show (serverPort config)
  
  putStrLn "\nExample 2: Form Validation"
  let validProfile = validateUserProfile "John Doe" "john@example.com" 30 (Just "https://example.com")
  let invalidProfile = validateUserProfile "J" "not-an-email" 10 (Just "example.com")
  
  case validProfile of
    ValidationSuccess profile -> putStrLn $ "Valid profile: " ++ show profile
    ValidationFailure errs -> putStrLn $ "Validation errors: " ++ show errs
  
  case invalidProfile of
    ValidationSuccess profile -> putStrLn $ "Valid profile: " ++ show profile
    ValidationFailure errs -> putStrLn $ "Validation errors: " ++ show errs
  
  putStrLn "\nExample 3: Command-line Argument Parsing"
  putStrLn $ "Parse ['add', 'item', '42']: " ++ show (parseArgs ["add", "item", "42"])
  putStrLn $ "Parse ['remove', 'item']: " ++ show (parseArgs ["remove", "item"])
  putStrLn $ "Parse ['unknown']: " ++ show (parseArgs ["unknown"])
  
  putStrLn "\nExample 4: Building Query Strings"
  putStrLn $ "Query: " ++ buildQuery "haskell" 10 (Just "recent") "relevance"
  putStrLn $ "Query without filter: " ++ buildQuery "haskell" 10 Nothing "relevance"
  
  putStrLn "\nExample 5: Combining Computations"
  putStrLn $ "Average of Just 10, Just 20, Just 30: " ++ show (computeAverage (Just 10) (Just 20) (Just 30))
  putStrLn $ "Average with a Nothing: " ++ show (computeAverage (Just 10) Nothing (Just 30))
  
  putStrLn "\nExample 6: Parallel vs Sequential Computation"
  putStrLn $ "Sequential: " ++ show sequentialComp
  putStrLn $ "Parallel: " ++ show parallelComp
