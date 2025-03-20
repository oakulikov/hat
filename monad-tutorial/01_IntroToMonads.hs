{-
  Introduction to Monads in Haskell
  
  Monads are a powerful abstraction in functional programming that help manage
  effects and sequencing of operations. They're particularly important in Haskell
  due to its pure functional nature.
-}

module IntroToMonads where

-- A monad in Haskell is a typeclass with two fundamental operations:
-- 1. return (or pure): Takes a value and wraps it in the monad
-- 2. >>= (bind): Chains monadic operations together

-- The Monad typeclass definition (simplified):
{-
class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
-}

-- To understand monads, let's start with a simple example: the Maybe monad

-- The Maybe type represents computations that might fail
-- data Maybe a = Nothing | Just a

-- Example: Division that handles division by zero
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing  -- Division by zero returns Nothing
safeDivide x y = Just (x / y)  -- Successful division returns Just result

-- Using Maybe monad to chain operations that might fail
chainedDivision :: Double -> Double -> Double -> Maybe Double
chainedDivision x y z =
  safeDivide x y >>= \result1 ->  -- If safeDivide x y succeeds, bind the result to result1
  safeDivide result1 z            -- Then try to divide result1 by z

-- The same operation using do-notation (syntactic sugar for monadic operations)
chainedDivisionDo :: Double -> Double -> Double -> Maybe Double
chainedDivisionDo x y z = do
  result1 <- safeDivide x y  -- If safeDivide x y fails, the whole computation returns Nothing
  safeDivide result1 z       -- Otherwise, try to divide result1 by z

-- Let's test these functions
test1 :: Maybe Double
test1 = chainedDivision 10 2 5  -- 10/2 = 5, 5/5 = 1, so this returns Just 1.0

test2 :: Maybe Double
test2 = chainedDivision 10 0 5  -- 10/0 is undefined, so this returns Nothing

-- The key insight: Monads provide a way to sequence operations while handling
-- context (in this case, the possibility of failure) automatically.

-- The Monad Laws (these must be satisfied by all monads):
-- 1. Left identity:  return a >>= f  ≡  f a
-- 2. Right identity: m >>= return    ≡  m
-- 3. Associativity:  (m >>= f) >>= g ≡  m >>= (\x -> f x >>= g)

main :: IO ()
main = do
  putStrLn "Testing Maybe Monad:"
  putStrLn $ "chainedDivision 10 2 5 = " ++ show test1
  putStrLn $ "chainedDivision 10 0 5 = " ++ show test2
