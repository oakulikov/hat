{-
  Creating Custom Monads in Haskell
  
  This module demonstrates how to create your own monads
  to encapsulate specific computational contexts.
-}

module CustomMonads where

import Control.Monad (ap)

-- Example 1: A simple Writer monad for logging
-- The Writer monad lets us perform computations while accumulating a log

newtype Writer log a = Writer { runWriter :: (a, log) }

-- Writer monad instance
instance Functor (Writer log) where
  fmap f (Writer (a, log)) = Writer (f a, log)

instance Monoid log => Applicative (Writer log) where
  pure a = Writer (a, mempty)
  (<*>) = ap

instance Monoid log => Monad (Writer log) where
  return = pure
  Writer (a, log1) >>= f = 
    let Writer (b, log2) = f a
    in Writer (b, log1 `mappend` log2)

-- Helper function to log a message
tell :: log -> Writer log ()
tell log = Writer ((), log)

-- Example usage of our Writer monad
logExample :: Writer [String] Int
logExample = do
  tell ["Starting computation"]
  let x = 10
  tell ["Set x to " ++ show x]
  let y = x * 2
  tell ["Computed y = " ++ show y]
  return y

-- Example 2: A simple State monad
-- The State monad lets us thread state through computations

newtype State s a = State { runState :: s -> (a, s) }

-- State monad instance
instance Functor (State s) where
  fmap f (State g) = State $ \s -> 
    let (a, s') = g s
    in (f a, s')

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) = ap

instance Monad (State s) where
  return = pure
  State act >>= f = State $ \s ->
    let (a, s') = act s
        State newAct = f a
    in newAct s'

-- Helper functions for the State monad
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Example usage of our State monad: a simple counter
counterExample :: State Int String
counterExample = do
  n <- get
  put (n + 1)
  m <- get
  modify (* 2)
  final <- get
  return $ "Initial: " ++ show n ++ ", After increment: " ++ show m ++ ", Final: " ++ show final

-- Example 3: A Reader monad
-- The Reader monad provides read-only access to an environment

newtype Reader env a = Reader { runReader :: env -> a }

instance Functor (Reader env) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader env) where
  pure a = Reader $ \_ -> a
  Reader f <*> Reader g = Reader $ \env -> f env (g env)

instance Monad (Reader env) where
  return = pure
  Reader g >>= f = Reader $ \env -> 
    let a = g env
        Reader h = f a
    in h env

-- Helper function for the Reader monad
ask :: Reader env env
ask = Reader id

-- Example usage of our Reader monad: configuration
data Config = Config { 
  username :: String,
  hostname :: String,
  port :: Int
}

connectionString :: Reader Config String
connectionString = do
  config <- ask
  return $ username config ++ "@" ++ hostname config ++ ":" ++ show (port config)

main :: IO ()
main = do
  putStrLn "Custom Monads Examples:"
  
  putStrLn "\nWriter Monad Example:"
  let (result, logs) = runWriter logExample
  putStrLn $ "Result: " ++ show result
  putStrLn "Logs:"
  mapM_ putStrLn logs
  
  putStrLn "\nState Monad Example:"
  let (output, finalState) = runState counterExample 5
  putStrLn $ "Output: " ++ output
  putStrLn $ "Final state: " ++ show finalState
  
  putStrLn "\nReader Monad Example:"
  let config = Config "user" "example.com" 8080
  let connStr = runReader connectionString config
  putStrLn $ "Connection string: " ++ connStr
