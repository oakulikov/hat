{-
  Applicative Functors and Monads in Haskell
  
  This module explores the relationship between applicative functors and monads,
  showing how they relate to each other and when to use each one.
-}

module ApplicativesAndMonads where

import Control.Applicative
import Control.Monad (liftM, ap)

-- The relationship between Applicative and Monad
-- In Haskell's type class hierarchy:
-- Functor -> Applicative -> Monad

-- For any monad, we can define its Applicative instance in terms of the monad operations:
-- pure = return
-- (<*>) = ap

-- Where ap is defined as:
-- ap :: Monad m => m (a -> b) -> m a -> m b
-- ap mf mx = do
--   f <- mf
--   x <- mx
--   return (f x)

-- Example 1: Implementing Applicative in terms of Monad
-- Let's define a simple type that will be both Applicative and Monad

newtype Identity a = Identity { runIdentity :: a }
  deriving Show

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- Applicative instance using monadic operations
instance Applicative Identity where
  pure = Identity
  (<*>) = ap  -- Using ap from Control.Monad

instance Monad Identity where
  return = Identity
  Identity x >>= f = f x

-- Example 2: When to use Applicative vs Monad
-- Applicative: When computations don't depend on previous results
-- Monad: When computations depend on previous results

-- Applicative example: Combining independent computations
independentComputations :: Maybe Int
independentComputations = 
  (+) <$> Just 3 <*> Just 5  -- Just 8

-- Monad example: Dependent computations
dependentComputations :: Maybe Int
dependentComputations = do
  x <- Just 3
  y <- if x > 0 then Just (x * 2) else Nothing
  return (x + y)  -- Just 9

-- Example 3: Applicative is more restrictive than Monad
-- Some things can only be expressed with Monad

-- This can be expressed with both Applicative and Monad
bothPossible :: Maybe Int
bothPossible = (+) <$> Just 3 <*> Just 5
-- Equivalent to:
-- do
--   x <- Just 3
--   y <- Just 5
--   return (x + y)

-- This can only be expressed with Monad
onlyMonad :: Maybe Int
onlyMonad = do
  x <- Just 3
  if x > 0
    then Just (x * 2)
    else Nothing
-- No direct Applicative equivalent

-- Example 4: Applicative allows for static analysis
-- Because Applicative doesn't allow for dynamic dependencies,
-- we can analyze the structure of the computation statically

-- A simple expression language
data Expr a = 
    Lit a
  | Add (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  deriving Show

-- Evaluate an expression, handling division by zero
evalExpr :: (Num a, Eq a, Fractional a) => Expr a -> Maybe a
evalExpr (Lit x) = Just x
evalExpr (Add e1 e2) = (+) <$> evalExpr e1 <*> evalExpr e2
evalExpr (Mul e1 e2) = (*) <$> evalExpr e1 <*> evalExpr e2
evalExpr (Div e1 e2) = do
  x <- evalExpr e1
  y <- evalExpr e2
  if y == 0 then Nothing else Just (x / y)
-- Note: Div uses monadic style because we need to check if y is 0

-- Example 5: Applicative for parallel computation, Monad for sequential
-- Applicative operations can conceptually be executed in parallel
-- Monadic operations must be executed sequentially

-- A type to track execution order
data Tracked a = Tracked [String] a
  deriving Show

instance Functor Tracked where
  fmap f (Tracked logs x) = Tracked logs (f x)

instance Applicative Tracked where
  pure x = Tracked [] x
  Tracked logs1 f <*> Tracked logs2 x = 
    Tracked (logs1 ++ logs2) (f x)

instance Monad Tracked where
  return = pure
  Tracked logs x >>= f = 
    let Tracked logs' y = f x
    in Tracked (logs ++ logs') y

-- Operations that log their execution
operation1 :: Tracked Int
operation1 = Tracked ["Operation 1 executed"] 5

operation2 :: Tracked Int
operation2 = Tracked ["Operation 2 executed"] 7

-- Applicative style (conceptually parallel)
applicativeStyle :: Tracked Int
applicativeStyle = (+) <$> operation1 <*> operation2
-- Result: Tracked ["Operation 1 executed", "Operation 2 executed"] 12

-- Monadic style (sequential)
monadicStyle :: Tracked Int
monadicStyle = do
  x <- operation1
  y <- operation2
  return (x + y)
-- Result: Tracked ["Operation 1 executed", "Operation 2 executed"] 12
-- (Same result, but the operations must be executed in sequence)

-- Example 6: Applicative for validation, Monad for error handling
-- Applicative can accumulate errors, Monad short-circuits on the first error

data Validation e a = 
    Success a
  | Failure e
  deriving Show

instance Functor (Validation e) where
  fmap f (Success x) = Success (f x)
  fmap _ (Failure e) = Failure e

instance Monoid e => Applicative (Validation e) where
  pure = Success
  
  Success f <*> Success x = Success (f x)
  Failure e1 <*> Failure e2 = Failure (e1 `mappend` e2)
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e

instance Monoid e => Monad (Validation e) where
  return = pure
  
  Success x >>= f = f x
  Failure e >>= _ = Failure e

-- Validation functions
checkPositive :: Int -> Validation [String] Int
checkPositive x
  | x > 0 = Success x
  | otherwise = Failure ["Value must be positive"]

checkEven :: Int -> Validation [String] Int
checkEven x
  | even x = Success x
  | otherwise = Failure ["Value must be even"]

-- Applicative validation (accumulates all errors)
applicativeValidation :: Int -> Validation [String] (Int, Int)
applicativeValidation x = 
  (,) <$> checkPositive x <*> checkEven x

-- Monadic validation (stops at first error)
monadicValidation :: Int -> Validation [String] (Int, Int)
monadicValidation x = do
  a <- checkPositive x
  b <- checkEven x
  return (a, b)

-- Example 7: Converting between Applicative and Monad
-- We can always convert Applicative code to Monadic code, but not vice versa

-- Applicative to Monad
applicativeToMonad :: Maybe Int
applicativeToMonad = (+) <$> Just 3 <*> Just 5

-- Equivalent monadic code
applicativeAsMonad :: Maybe Int
applicativeAsMonad = do
  x <- Just 3
  y <- Just 5
  return (x + y)

-- Monad to Applicative (only works for some cases)
monadicToApplicative :: Maybe Int
monadicToApplicative = do
  x <- Just 3
  return (x + 5)

-- Equivalent applicative code
monadicAsApplicative :: Maybe Int
monadicAsApplicative = (+ 5) <$> Just 3

-- Example 8: Implementing Monad in terms of Applicative and join
-- join :: Monad m => m (m a) -> m a
-- join x = x >>= id

-- Implementing (>>=) using (<*>) and join
bindUsingApplicativeAndJoin :: Monad m => m a -> (a -> m b) -> m b
bindUsingApplicativeAndJoin mx f = join (f <$> mx)
  where
    join :: Monad m => m (m a) -> m a
    join x = x >>= id

main :: IO ()
main = do
  putStrLn "Applicative Functors and Monads Examples:"
  
  putStrLn "\nExample 1: Implementing Applicative in terms of Monad"
  print $ runIdentity $ (+) <$> Identity 3 <*> Identity 5  -- 8
  
  putStrLn "\nExample 2: When to use Applicative vs Monad"
  putStrLn $ "Independent computations: " ++ show independentComputations  -- Just 8
  putStrLn $ "Dependent computations: " ++ show dependentComputations  -- Just 9
  
  putStrLn "\nExample 3: Applicative is more restrictive than Monad"
  putStrLn $ "Both possible: " ++ show bothPossible  -- Just 8
  putStrLn $ "Only monad: " ++ show onlyMonad  -- Just 6
  
  putStrLn "\nExample 4: Expression Evaluation"
  let expr = Div (Add (Lit 10) (Lit 5)) (Mul (Lit 3) (Lit 1))
  putStrLn $ "Evaluate (10 + 5) / (3 * 1): " ++ show (evalExpr expr)  -- Just 5.0
  
  let exprDivByZero = Div (Lit 10) (Add (Lit 5) (Lit (-5)))
  putStrLn $ "Evaluate 10 / (5 + (-5)): " ++ show (evalExpr exprDivByZero)  -- Nothing
  
  putStrLn "\nExample 5: Parallel vs Sequential Execution"
  putStrLn $ "Applicative style: " ++ show applicativeStyle
  putStrLn $ "Monadic style: " ++ show monadicStyle
  
  putStrLn "\nExample 6: Validation"
  putStrLn $ "Applicative validation of 6: " ++ show (applicativeValidation 6)  -- Success (6, 6)
  putStrLn $ "Applicative validation of -5: " ++ 
             show (applicativeValidation (-5))  -- Failure ["Value must be positive", "Value must be even"]
  
  putStrLn $ "Monadic validation of 6: " ++ show (monadicValidation 6)  -- Success (6, 6)
  putStrLn $ "Monadic validation of -5: " ++ 
             show (monadicValidation (-5))  -- Failure ["Value must be positive"]
  
  putStrLn "\nExample 7: Converting between Applicative and Monad"
  putStrLn $ "Applicative to Monad: " ++ show applicativeToMonad  -- Just 8
  putStrLn $ "Applicative as Monad: " ++ show applicativeAsMonad  -- Just 8
  putStrLn $ "Monad to Applicative: " ++ show monadicToApplicative  -- Just 8
  putStrLn $ "Monad as Applicative: " ++ show monadicAsApplicative  -- Just 8
  
  putStrLn "\nExample 8: Implementing Monad in terms of Applicative and join"
  let result = bindUsingApplicativeAndJoin (Just 3) (\x -> Just (x * 2))
  putStrLn $ "bind using Applicative and join: " ++ show result  -- Just 6
