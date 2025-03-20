{-
  The IO Monad in Haskell
  
  The IO monad is one of the most important monads in Haskell.
  It allows us to perform input/output operations while maintaining
  referential transparency in the rest of the program.
-}

module IOMonad where

-- The IO monad encapsulates computations that:
-- 1. May have side effects (like printing to the console)
-- 2. May depend on the external world (like reading user input)
-- 3. May change the state of the external world

-- Simple IO actions
helloWorld :: IO ()
helloWorld = putStrLn "Hello, World!"

-- Sequencing IO actions
greetUser :: IO ()
greetUser = do
  putStrLn "What is your name?"
  name <- getLine  -- Bind the result of getLine to 'name'
  putStrLn $ "Hello, " ++ name ++ "!"

-- Using >>= (bind) explicitly
greetUserBind :: IO ()
greetUserBind =
  putStrLn "What is your name?" >>
  getLine >>= \name ->
  putStrLn $ "Hello, " ++ name ++ "!"

-- IO actions that return values
readNumber :: IO Int
readNumber = do
  putStrLn "Enter a number:"
  input <- getLine
  return (read input)  -- 'return' wraps the value in the IO monad

-- Combining IO actions that return values
addTwoNumbers :: IO Int
addTwoNumbers = do
  putStrLn "Let's add two numbers:"
  num1 <- readNumber
  num2 <- readNumber
  let sum = num1 + num2
  putStrLn $ "The sum is: " ++ show sum
  return sum

-- Using IO with pure functions
applyFunction :: (Int -> Int) -> IO ()
applyFunction f = do
  num <- readNumber
  let result = f num
  putStrLn $ "The result is: " ++ show result

-- Example: A simple interactive calculator
calculator :: IO ()
calculator = do
  putStrLn "Simple Calculator"
  putStrLn "1. Add"
  putStrLn "2. Subtract"
  putStrLn "3. Multiply"
  putStrLn "4. Divide"
  putStrLn "Enter your choice (1-4):"
  
  choice <- getLine
  
  putStrLn "Enter first number:"
  num1 <- readLn :: IO Double
  
  putStrLn "Enter second number:"
  num2 <- readLn :: IO Double
  
  let result = case choice of
        "1" -> num1 + num2
        "2" -> num1 - num2
        "3" -> num1 * num2
        "4" -> if num2 /= 0 then num1 / num2 else error "Division by zero"
        _   -> error "Invalid choice"
  
  putStrLn $ "Result: " ++ show result

main :: IO ()
main = do
  putStrLn "IO Monad Examples:"
  
  putStrLn "\nExample 1: Hello World"
  helloWorld
  
  putStrLn "\nExample 2: Greeting (uncomment to run interactively)"
  -- greetUser  -- Commented out for non-interactive execution
  
  putStrLn "\nExample 3: Function Application"
  putStrLn "Applying the function (x^2) to a number (uncomment to run interactively)"
  -- applyFunction (\x -> x * x)  -- Commented out for non-interactive execution
  
  putStrLn "\nExample 4: Calculator (uncomment to run interactively)"
  -- calculator  -- Commented out for non-interactive execution
  
  putStrLn "\nNote: Uncomment the interactive examples in the code to try them out."
