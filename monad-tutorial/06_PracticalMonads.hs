{-
  Practical Applications of Monads in Haskell
  
  This module demonstrates how monads are used to solve real-world
  programming problems in a clean and composable way.
  
  Note: This file implements simplified versions of monad transformers
  without requiring the 'transformers' package, and uses association lists
  instead of Data.Map.
-}

module PracticalMonads where

import Control.Monad (when, forM)
import Data.Maybe (fromMaybe)
import Data.List (lookup)

-- Example 1: Error handling with the Either monad
-- Either is commonly used for operations that can fail with an error message

data UserError = 
    InvalidName
  | InvalidAge
  | UserNotFound
  deriving Show

validateName :: String -> Either UserError String
validateName name
  | length name < 2 = Left InvalidName
  | otherwise = Right name

validateAge :: Int -> Either UserError Int
validateAge age
  | age < 0 || age > 150 = Left InvalidAge
  | otherwise = Right age

-- Using Either to chain validations
validateUser :: String -> Int -> Either UserError (String, Int)
validateUser name age = do
  validName <- validateName name
  validAge <- validateAge age
  return (validName, validAge)

-- Example 2: Configuration management with Reader monad

data AppConfig = AppConfig {
  appName :: String,
  maxConnections :: Int,
  timeout :: Int
}

-- Simple Reader monad implementation
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader r) where
  pure x = Reader (const x)
  Reader f <*> Reader g = Reader (\r -> f r (g r))

instance Monad (Reader r) where
  return = pure
  Reader g >>= f = Reader (\r -> runReader (f (g r)) r)

-- Reader operations
ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

local :: (r -> r) -> Reader r a -> Reader r a
local f (Reader g) = Reader (g . f)

type ConfigM = Reader AppConfig

getAppName :: ConfigM String
getAppName = asks appName

getMaxConnections :: ConfigM Int
getMaxConnections = asks maxConnections

getTimeout :: ConfigM Int
getTimeout = asks timeout

-- A function that uses configuration
initializeApp :: ConfigM String
initializeApp = do
  name <- getAppName
  conns <- getMaxConnections
  time <- getTimeout
  return $ "Initializing " ++ name ++ " with " ++ 
           show conns ++ " connections and " ++ 
           show time ++ "ms timeout"

-- Example 3: Stateful computations with State monad

-- A simple game state
data GameState = GameState {
  playerHealth :: Int,
  playerScore :: Int,
  gameLevel :: Int
} deriving Show

-- Simple State monad implementation
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> 
    let (a, s') = g s
    in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State f <*> State g = State $ \s ->
    let (h, s') = f s
        (a, s'') = g s'
    in (h a, s'')

instance Monad (State s) where
  return = pure
  State g >>= f = State $ \s ->
    let (a, s') = g s
    in runState (f a) s'

-- State operations
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

type Game = State GameState

-- Game actions
takeDamage :: Int -> Game ()
takeDamage amount = modify $ \s -> 
  s { playerHealth = max 0 (playerHealth s - amount) }

addScore :: Int -> Game ()
addScore points = modify $ \s ->
  s { playerScore = playerScore s + points }

levelUp :: Game ()
levelUp = modify $ \s ->
  s { gameLevel = gameLevel s + 1, playerHealth = 100 }

-- A game sequence
gameSequence :: Game String
gameSequence = do
  addScore 100
  takeDamage 30
  levelUp
  addScore 200
  takeDamage 50
  
  -- Get final state
  GameState health score level <- get
  return $ "Game over! Final stats: Health=" ++ show health ++ 
           ", Score=" ++ show score ++ ", Level=" ++ show level

-- Example 4: Dependency injection with Reader monad

-- Simple ReaderT monad transformer implementation
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT $ \r -> fmap f (g r)

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT $ \_ -> pure a
  ReaderT f <*> ReaderT g = ReaderT $ \r -> f r <*> g r

instance Monad m => Monad (ReaderT r m) where
  return = pure
  ReaderT g >>= f = ReaderT $ \r -> do
    a <- g r
    runReaderT (f a) r

-- Lift operation for ReaderT
lift :: Monad m => m a -> ReaderT r m a
lift ma = ReaderT $ \_ -> ma

-- Services
class Logger m where
  logInfo :: String -> m ()
  logError :: String -> m ()

class Database m where
  getUser :: Int -> m (Maybe String)
  saveUser :: Int -> String -> m Bool

-- Implementation with IO
data Services = Services {
  loggerImpl :: String -> IO (),
  errorLoggerImpl :: String -> IO (),
  getUserImpl :: Int -> IO (Maybe String),
  saveUserImpl :: Int -> String -> IO Bool
}

type ServiceM = ReaderT Services IO

instance Logger ServiceM where
  logInfo msg = do
    logger <- ReaderT $ \r -> return (loggerImpl r)
    lift $ logger msg
  
  logError msg = do
    errorLogger <- ReaderT $ \r -> return (errorLoggerImpl r)
    lift $ errorLogger msg

instance Database ServiceM where
  getUser id = do
    getUserFn <- ReaderT $ \r -> return (getUserImpl r)
    lift $ getUserFn id
  
  saveUser id name = do
    saveUserFn <- ReaderT $ \r -> return (saveUserImpl r)
    lift $ saveUserFn id name

-- Business logic using the services
userService :: Int -> String -> ServiceM Bool
userService userId userName = do
  logInfo $ "Attempting to save user: " ++ userName
  
  existingUser <- getUser userId
  case existingUser of
    Just name -> do
      logInfo $ "User already exists: " ++ name
      return False
    
    Nothing -> do
      success <- saveUser userId userName
      if success
        then logInfo $ "User saved successfully: " ++ userName
        else logError $ "Failed to save user: " ++ userName
      return success

-- Example 5: Parsing with the Maybe monad

type Parser a = String -> Maybe (a, String)

-- Basic parsers
charP :: Char -> Parser Char
charP c (x:xs) | x == c = Just (c, xs)
charP _ _ = Nothing

stringP :: String -> Parser String
stringP str input = 
  case str of
    [] -> Just (str, input)
    (c:cs) -> do
      (_, rest) <- charP c input
      (result, remaining) <- stringP cs rest
      return (c:result, remaining)

intP :: Parser Int
intP input = do
  let (digits, rest) = span isDigit input
  if null digits
    then Nothing
    else Just (read digits, rest)
  where
    isDigit c = c >= '0' && c <= '9'

-- Combining parsers
(<|>) :: Parser a -> Parser a -> Parser a
(p1 <|> p2) input = 
  case p1 input of
    Just result -> Just result
    Nothing -> p2 input

-- Example parser for a simple expression
exprP :: Parser Int
exprP input = 
  case intP input of
    Just (n1, rest1) -> 
      case stringP "+" rest1 of
        Just (_, rest2) -> 
          case intP rest2 of
            Just (n2, rest3) -> Just (n1 + n2, rest3)
            Nothing -> Just (n1, rest1)  -- If no second number, return first
        Nothing -> Just (n1, rest1)  -- If no plus sign, return first number
    Nothing -> Nothing  -- If no first number, parsing fails

-- Example 6: Building a mini-interpreter with monads

-- A simple expression language
data Expr = 
    Lit Int
  | Add Expr Expr
  | Var String
  | Let String Expr Expr  -- let x = e1 in e2
  deriving Show

-- Environment for variable bindings using association list
type Env = [(String, Int)]

-- Interpreter using the Reader monad
type Eval a = Reader Env a

eval :: Expr -> Eval Int
eval (Lit n) = return n
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)
eval (Var name) = do
  env <- ask
  case lookup name env of
    Just val -> return val
    Nothing -> return 0  -- Default value for undefined variables
eval (Let name e1 e2) = do
  val <- eval e1
  local ((name, val):) (eval e2)

-- Example expressions
expr1 :: Expr
expr1 = Add (Lit 5) (Lit 10)

expr2 :: Expr
expr2 = Let "x" (Lit 10) (Add (Var "x") (Lit 5))

expr3 :: Expr
expr3 = Let "x" (Lit 5) 
          (Let "y" (Add (Var "x") (Lit 10))
            (Add (Var "x") (Var "y")))

main :: IO ()
main = do
  putStrLn "Practical Monad Examples:"
  
  putStrLn "\nExample 1: Error Handling with Either"
  putStrLn $ "Validating ('John', 30): " ++ show (validateUser "John" 30)
  putStrLn $ "Validating ('J', 30): " ++ show (validateUser "J" 30)
  putStrLn $ "Validating ('John', 200): " ++ show (validateUser "John" 200)
  
  putStrLn "\nExample 2: Configuration with Reader"
  let config = AppConfig "MyApp" 100 5000
  putStrLn $ runReader initializeApp config
  
  putStrLn "\nExample 3: Game State with State"
  let initialState = GameState 100 0 1
  let (result, finalState) = runState gameSequence initialState
  putStrLn result
  putStrLn $ "Final state: " ++ show finalState
  
  putStrLn "\nExample 4: Dependency Injection (would require IO to run)"
  
  putStrLn "\nExample 5: Parsing"
  putStrLn $ "Parsing '123': " ++ show (intP "123")
  putStrLn $ "Parsing '123+456': " ++ show (exprP "123+456")
  
  putStrLn "\nExample 6: Mini-Interpreter"
  putStrLn $ "Evaluating 5 + 10: " ++ show (runReader (eval expr1) [])
  putStrLn $ "Evaluating let x = 10 in x + 5: " ++ show (runReader (eval expr2) [])
  putStrLn $ "Evaluating let x = 5 in let y = x + 10 in x + y: " ++ 
             show (runReader (eval expr3) [])
