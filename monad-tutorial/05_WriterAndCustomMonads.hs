{-
  Writer и пользовательские монады в Haskell
  
  В этом файле мы рассмотрим подробнее монаду Writer и создание собственных монад.
-}

module Main where

import Control.Monad (liftM, ap)
import Control.Monad.Writer (Writer, runWriter, tell, execWriter, censor, listen)
import Data.Monoid (Sum(..), Product(..), Any(..), All(..))

-- Монада Writer
-- Writer представляет вычисление, которое может производить дополнительный вывод
-- newtype Writer w a = Writer { runWriter :: (a, w) }

-- Реализация Monad для Writer (уже встроена в Control.Monad.Writer)
{-
instance (Monoid w) => Monad (Writer w) where
  return a = Writer (a, mempty)
  Writer (a, w) >>= f = 
    let (b, w') = runWriter (f a)
    in Writer (b, w `mappend` w')
-}

-- Пример использования Writer для логирования
logNumber :: Int -> Writer [String] Int
logNumber n = do
  tell ["Processing number: " ++ show n]
  return (n * 2)

-- Цепочка вычислений с Writer
processNumbers :: [Int] -> Writer [String] [Int]
processNumbers [] = do
  tell ["Processing empty list"]
  return []
processNumbers (x:xs) = do
  tell ["Processing list with head: " ++ show x]
  y <- logNumber x
  ys <- processNumbers xs
  tell ["Finished processing list"]
  return (y:ys)

-- Пример использования Writer с разными моноидами
sumWithLog :: [Int] -> Writer (Sum Int) [Int]
sumWithLog xs = do
  tell (Sum (sum xs))
  return xs

productWithLog :: [Int] -> Writer (Product Int) [Int]
productWithLog xs = do
  tell (Product (product xs))
  return xs

anyEvenWithLog :: [Int] -> Writer Any [Int]
anyEvenWithLog xs = do
  tell (Any (any even xs))
  return xs

allPositiveWithLog :: [Int] -> Writer All [Int]
allPositiveWithLog xs = do
  tell (All (all (> 0) xs))
  return xs

-- Пример использования censor для модификации лога
censored :: Writer [String] Int
censored = censor (map (const "CENSORED")) $ do
  tell ["This is a secret"]
  tell ["Another secret"]
  return 42

-- Пример использования listen для доступа к логу
withListen :: Writer [String] (Int, [String])
withListen = listen $ do
  tell ["First message"]
  tell ["Second message"]
  return 42

-- Создание собственной монады
-- Пример: Maybe с сообщениями об ошибках
data MaybeError a = Nothing' | Just' a String deriving (Show)

-- Реализация Functor для MaybeError
instance Functor MaybeError where
  fmap _ Nothing' = Nothing'
  fmap f (Just' x msg) = Just' (f x) msg

-- Реализация Applicative для MaybeError
instance Applicative MaybeError where
  pure x = Just' x ""
  Nothing' <*> _ = Nothing'
  (Just' f msg1) <*> (Just' x msg2) = Just' (f x) (msg1 ++ msg2)
  _ <*> Nothing' = Nothing'

-- Реализация Monad для MaybeError
instance Monad MaybeError where
  return = pure
  Nothing' >>= _ = Nothing'
  (Just' x msg1) >>= f = 
    case f x of
      Nothing' -> Nothing'
      Just' y msg2 -> Just' y (msg1 ++ msg2)

-- Функции для работы с MaybeError
failWithError :: String -> MaybeError a
failWithError msg = Nothing'

succeedWithMessage :: a -> String -> MaybeError a
succeedWithMessage x msg = Just' x msg

-- Пример использования MaybeError
safeDivide :: Double -> Double -> MaybeError Double
safeDivide _ 0 = failWithError "Division by zero"
safeDivide x y = succeedWithMessage (x / y) ("Divided " ++ show x ++ " by " ++ show y ++ ". ")

safeRoot :: Double -> MaybeError Double
safeRoot x | x < 0 = failWithError "Negative number under square root"
           | otherwise = succeedWithMessage (sqrt x) ("Took square root of " ++ show x ++ ". ")

complexCalculation :: Double -> Double -> MaybeError Double
complexCalculation x y = do
  result1 <- safeDivide x y
  result2 <- safeRoot result1
  return (result2 * 2)

-- Еще один пример собственной монады: Tree
data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

-- Реализация Functor для Tree
instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

-- Реализация Applicative для Tree
instance Applicative Tree where
  pure = Leaf
  (Leaf f) <*> tree = fmap f tree
  (Branch left right) <*> tree = Branch (left <*> tree) (right <*> tree)

-- Реализация Monad для Tree
instance Monad Tree where
  return = pure
  (Leaf x) >>= f = f x
  (Branch left right) >>= f = Branch (left >>= f) (right >>= f)

-- Пример использования Tree как монады
treeExample :: Tree Int
treeExample = do
  x <- Branch (Leaf 1) (Leaf 2)
  y <- Branch (Leaf 3) (Leaf 4)
  return (x + y)

-- Пример 1: Использование Writer для логирования
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Использование Writer для логирования"
  
  let (result, logs) = runWriter (processNumbers [1, 2, 3])
  
  putStrLn $ "Результат: " ++ show result
  putStrLn "Логи:"
  mapM_ putStrLn logs

-- Пример 2: Использование Writer с разными моноидами
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Использование Writer с разными моноидами"
  
  let numbers = [1, 2, 3, 4, 5]
  
  let (result1, Sum sum') = runWriter (sumWithLog numbers)
  putStrLn $ "Сумма чисел " ++ show result1 ++ " = " ++ show sum'
  
  let (result2, Product product') = runWriter (productWithLog numbers)
  putStrLn $ "Произведение чисел " ++ show result2 ++ " = " ++ show product'
  
  let (result3, Any anyEven) = runWriter (anyEvenWithLog numbers)
  putStrLn $ "Есть ли четные числа в " ++ show result3 ++ "? " ++ show anyEven
  
  let (result4, All allPositive) = runWriter (allPositiveWithLog numbers)
  putStrLn $ "Все ли числа положительные в " ++ show result4 ++ "? " ++ show allPositive

-- Пример 3: Использование censor и listen
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Использование censor и listen"
  
  let (result1, logs1) = runWriter censored
  putStrLn $ "Результат с цензурой: " ++ show result1
  putStrLn "Логи с цензурой:"
  mapM_ putStrLn logs1
  
  let (result2, logs2) = runWriter withListen
  putStrLn $ "Результат с listen: " ++ show result2
  putStrLn "Логи из listen:"
  mapM_ putStrLn (snd result2)
  putStrLn "Оригинальные логи:"
  mapM_ putStrLn logs2

-- Пример 4: Использование собственной монады MaybeError
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Использование собственной монады MaybeError"
  
  putStrLn $ "safeDivide 10 2 = " ++ show (safeDivide 10 2)
  putStrLn $ "safeDivide 10 0 = " ++ show (safeDivide 10 0)
  
  putStrLn $ "safeRoot 16 = " ++ show (safeRoot 16)
  putStrLn $ "safeRoot (-16) = " ++ show (safeRoot (-16))
  
  putStrLn $ "complexCalculation 16 2 = " ++ show (complexCalculation 16 2)
  putStrLn $ "complexCalculation 16 0 = " ++ show (complexCalculation 16 0)
  putStrLn $ "complexCalculation (-16) 2 = " ++ show (complexCalculation (-16) 2)

-- Пример 5: Использование собственной монады Tree
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Использование собственной монады Tree"
  
  let tree1 = Branch (Leaf 1) (Leaf 2)
  let tree2 = Branch (Leaf 3) (Leaf 4)
  
  putStrLn $ "tree1 = " ++ show tree1
  putStrLn $ "tree2 = " ++ show tree2
  
  putStrLn $ "fmap (+1) tree1 = " ++ show (fmap (+1) tree1)
  
  putStrLn $ "tree1 >>= (\\x -> Leaf (x * 2)) = " ++ show (tree1 >>= (\x -> Leaf (x * 2)))
  
  putStrLn $ "treeExample = " ++ show treeExample

-- Пример 6: Законы монад для собственных монад
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Законы монад для собственных монад"
  
  putStrLn "При создании собственной монады необходимо убедиться, что она удовлетворяет трем законам монад:"
  
  putStrLn "\n1. Левая единица: return a >>= f ≡ f a"
  putStrLn "   Для MaybeError:"
  putStrLn "   return 5 >>= safeDivide 10 должно быть эквивалентно safeDivide 10 5"
  
  putStrLn "\n2. Правая единица: m >>= return ≡ m"
  putStrLn "   Для MaybeError:"
  putStrLn "   safeDivide 10 2 >>= return должно быть эквивалентно safeDivide 10 2"
  
  putStrLn "\n3. Ассоциативность: (m >>= f) >>= g ≡ m >>= (\\x -> f x >>= g)"
  putStrLn "   Для MaybeError:"
  putStrLn "   (safeDivide 10 2 >>= safeRoot) >>= (\\x -> return (x * 2))"
  putStrLn "   должно быть эквивалентно"
  putStrLn "   safeDivide 10 2 >>= (\\x -> safeRoot x >>= (\\y -> return (y * 2)))"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Writer и пользовательские монады в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о Writer и пользовательских монадах:"
  putStrLn "1. Writer представляет вычисление, которое может производить дополнительный вывод"
  putStrLn "2. Writer использует моноиды для накопления вывода (списки, Sum, Product, Any, All и т.д.)"
  putStrLn "3. Для создания собственной монады необходимо реализовать типовые классы Functor, Applicative и Monad"
  putStrLn "4. Собственная монада должна удовлетворять трем законам монад"
  putStrLn "5. Монады позволяют абстрагировать и инкапсулировать различные эффекты вычислений"
