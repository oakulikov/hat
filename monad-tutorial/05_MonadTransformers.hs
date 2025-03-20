{-
  Monad Transformers in Haskell
  
  Monad transformers allow us to combine multiple monads to create
  a monad stack that has the features of all the combined monads.
  
  Note: This file implements simplified versions of monad transformers
  without requiring the 'transformers' package.
-}

module MonadTransformers where

import Control.Monad (when)
import Data.Maybe (isNothing)

-- Our own implementation of the basic monad transformers

-- MaybeT: Adds possibility of failure to any monad
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ do
    mValue <- ma
    return $ fmap f mValue

instance Monad m => Applicative (MaybeT m) where
  pure x = MaybeT $ return (Just x)
  (MaybeT mf) <*> (MaybeT ma) = MaybeT $ do
    maybeF <- mf
    case maybeF of
      Nothing -> return Nothing
      Just f -> do
        maybeA <- ma
        return $ fmap f maybeA

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ do
    maybeA <- ma
    case maybeA of
      Nothing -> return Nothing
      Just a -> runMaybeT (f a)

-- StateT: Adds state to any monad
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = do
  (_, s') <- runStateT m s
  return s'

instance Monad m => Functor (StateT s m) where
  fmap f (StateT g) = StateT $ \s -> do
    (a, s') <- g s
    return (f a, s')

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (StateT mf) <*> (StateT ma) = StateT $ \s -> do
    (f, s') <- mf s
    (a, s'') <- ma s'
    return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure
  (StateT ma) >>= f = StateT $ \s -> do
    (a, s') <- ma s
    runStateT (f a) s'

-- State operations
get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)

-- ReaderT: Adds read-only environment to any monad
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT $ \r -> fmap f (g r)

instance Monad m => Applicative (ReaderT r m) where
  pure a = ReaderT $ \_ -> return a
  (ReaderT mf) <*> (ReaderT ma) = ReaderT $ \r -> do
    f <- mf r
    a <- ma r
    return (f a)

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT ma) >>= f = ReaderT $ \r -> do
    a <- ma r
    runReaderT (f a) r

-- Reader operations
ask :: Monad m => ReaderT r m r
ask = ReaderT return

-- WriterT: Adds accumulation to any monad
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance (Monad m, Monoid w) => Functor (WriterT w m) where
  fmap f (WriterT ma) = WriterT $ do
    (a, w) <- ma
    return (f a, w)

instance (Monad m, Monoid w) => Applicative (WriterT w m) where
  pure a = WriterT $ return (a, mempty)
  (WriterT mf) <*> (WriterT ma) = WriterT $ do
    (f, w1) <- mf
    (a, w2) <- ma
    return (f a, w1 `mappend` w2)

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  return = pure
  (WriterT ma) >>= f = WriterT $ do
    (a, w1) <- ma
    (b, w2) <- runWriterT (f a)
    return (b, w1 `mappend` w2)

-- Writer operations
tell :: (Monad m, Monoid w) => w -> WriterT w m ()
tell w = WriterT $ return ((), w)

-- Lift: Lifts a computation from the base monad to the transformed monad
class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans MaybeT where
  lift ma = MaybeT $ do
    a <- ma
    return (Just a)

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do
    a <- ma
    return (a, s)

instance MonadTrans (ReaderT r) where
  lift ma = ReaderT $ \_ -> ma

instance Monoid w => MonadTrans (WriterT w) where
  lift ma = WriterT $ do
    a <- ma
    return (a, mempty)

-- Example 1: MaybeT transformer with IO
-- This combines the Maybe monad (for possible failure) with IO

-- A function that might fail
lookupUser :: String -> MaybeT IO String
lookupUser "admin" = return "Administrator"
lookupUser "guest" = return "Guest User"
lookupUser _ = MaybeT $ return Nothing  -- User not found

-- Using the MaybeT transformer
userLookupExample :: IO ()
userLookupExample = do
  putStrLn "MaybeT Example:"
  
  -- Run the MaybeT computation
  result1 <- runMaybeT $ lookupUser "admin"
  case result1 of
    Just name -> putStrLn $ "Found user: " ++ name
    Nothing -> putStrLn "User not found"
  
  result2 <- runMaybeT $ lookupUser "unknown"
  case result2 of
    Just name -> putStrLn $ "Found user: " ++ name
    Nothing -> putStrLn "User not found"

-- Example 2: StateT transformer with IO
-- This combines the State monad (for mutable state) with IO

type GameState = StateT Int IO

-- A simple game where the state is the player's score
updateScore :: Int -> GameState ()
updateScore points = do
  currentScore <- get
  let newScore = currentScore + points
  put newScore
  -- Using lift to access the IO monad inside StateT
  lift $ putStrLn $ "Score updated: " ++ show currentScore ++ " -> " ++ show newScore

gameExample :: IO ()
gameExample = do
  putStrLn "\nStateT Example:"
  
  -- Run the StateT computation with initial state 0
  finalState <- execStateT game 0
  putStrLn $ "Final score: " ++ show finalState
  where
    game :: GameState ()
    game = do
      updateScore 10  -- Player earns 10 points
      updateScore 5   -- Player earns 5 points
      updateScore (-3) -- Player loses 3 points

-- Example 3: ReaderT and WriterT combined
-- This combines Reader (for environment) and Writer (for logging)

type AppConfig = String
type AppMonad a = ReaderT AppConfig (WriterT [String] IO) a

logMessage :: String -> AppMonad ()
logMessage msg = lift $ tell [msg]

getConfig :: AppMonad AppConfig
getConfig = ask

appExample :: AppMonad ()
appExample = do
  config <- getConfig
  logMessage $ "Starting application with config: " ++ config
  lift $ lift $ putStrLn "Doing some work..."  -- Lift IO action through both transformers
  logMessage "Work completed"

runAppExample :: IO ()
runAppExample = do
  putStrLn "\nReaderT + WriterT Example:"
  
  -- Run the combined monad stack
  (_, logs) <- runWriterT $ runReaderT appExample "Development Mode"
  
  putStrLn "Application logs:"
  mapM_ putStrLn logs

-- Example 4: A more complex example - a simple database with validation

-- Our database operations might fail, need configuration, and should be logged
type DbConfig = String
type DbMonad a = ReaderT DbConfig (WriterT [String] (MaybeT IO)) a

-- Simulate database operations
connectToDb :: DbMonad ()
connectToDb = do
  config <- ask
  logDb $ "Connecting to database with config: " ++ config

queryDb :: String -> DbMonad [String]
queryDb query = do
  logDb $ "Executing query: " ++ query
  -- Simulate a query that returns results
  return ["Result 1", "Result 2"]

validateResult :: [String] -> DbMonad [String]
validateResult results = do
  if null results
    then do
      logDb "Validation failed: empty result set"
      lift $ lift $ MaybeT $ return Nothing  -- Fail the computation
    else do
      logDb $ "Validation passed: " ++ show (length results) ++ " results"
      return results

logDb :: String -> DbMonad ()
logDb msg = lift $ tell [msg]

dbExample :: DbMonad [String]
dbExample = do
  connectToDb
  results <- queryDb "SELECT * FROM users"
  validateResult results

runDbExample :: IO ()
runDbExample = do
  putStrLn "\nComplex Monad Stack Example:"
  
  -- Run the complex monad stack
  result <- runMaybeT $ runWriterT $ runReaderT dbExample "Production DB"
  
  case result of
    Just (results, logs) -> do
      putStrLn "Database operation succeeded"
      putStrLn "Results:"
      mapM_ putStrLn results
      putStrLn "Logs:"
      mapM_ putStrLn logs
    Nothing -> putStrLn "Database operation failed"

main :: IO ()
main = do
  putStrLn "Monad Transformer Examples:"
  userLookupExample
  gameExample
  runAppExample
  runDbExample
