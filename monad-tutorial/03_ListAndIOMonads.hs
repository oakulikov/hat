{-
  List и IO монады в Haskell
  
  В этом файле мы рассмотрим подробнее монады List и IO и их применение.
-}

module Main where

import Control.Monad (liftM, ap, filterM, foldM)
import Data.Char (toUpper)

-- Монада List
-- List представляет вычисление с множеством результатов
-- data [] a = [] | a : [a]

-- Реализация Monad для List (уже встроена в Haskell)
{-
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
-}

-- Пример использования List для недетерминированных вычислений
-- Функция, которая возвращает все возможные результаты деления числа
allDivisions :: Int -> [Int]
allDivisions n = [d | d <- [1..n], n `mod` d == 0]

-- Функция, которая возвращает все возможные результаты умножения числа
allMultiplications :: Int -> [Int]
allMultiplications n = [n * m | m <- [1..5]]

-- Цепочка вычислений с List
allOperations :: Int -> [Int]
allOperations n = do
  d <- allDivisions n
  m <- allMultiplications d
  return (m + 1)

-- То же самое, но с использованием оператора >>=
allOperationsWithBind :: Int -> [Int]
allOperationsWithBind n =
  allDivisions n >>= \d ->
  allMultiplications d >>= \m ->
  return (m + 1)

-- Использование функции guard для фильтрации результатов
guardExample :: [Int]
guardExample = do
  x <- [1..10]
  guard (x `mod` 2 == 0)
  return (x * x)
  where
    guard True = [()]
    guard False = []

-- Монада IO
-- IO представляет вычисление, которое может выполнять ввод-вывод
-- Тип IO a представляет вычисление, которое может иметь побочные эффекты и возвращает значение типа a

-- Реализация Monad для IO (уже встроена в Haskell)
{-
instance Monad IO where
  return = returnIO
  (>>=) = bindIO
-}

-- Пример использования IO для ввода-вывода
getNameAndGreet :: IO ()
getNameAndGreet = do
  putStrLn "Как вас зовут?"
  name <- getLine
  putStrLn $ "Привет, " ++ name ++ "!"

-- Функция, которая запрашивает число и возвращает его
getNumber :: IO Int
getNumber = do
  putStrLn "Введите число:"
  line <- getLine
  return (read line)

-- Цепочка вычислений с IO
calculateSum :: IO ()
calculateSum = do
  putStrLn "Сейчас мы вычислим сумму двух чисел."
  x <- getNumber
  y <- getNumber
  let sum = x + y
  putStrLn $ "Сумма " ++ show x ++ " и " ++ show y ++ " равна " ++ show sum

-- То же самое, но с использованием оператора >>=
calculateSumWithBind :: IO ()
calculateSumWithBind =
  putStrLn "Сейчас мы вычислим сумму двух чисел." >>
  getNumber >>= \x ->
  getNumber >>= \y ->
  let sum = x + y in
  putStrLn $ "Сумма " ++ show x ++ " и " ++ show y ++ " равна " ++ show sum

-- Пример 1: Использование List для недетерминированных вычислений
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Использование List для недетерминированных вычислений"
  
  putStrLn $ "allDivisions 12 = " ++ show (allDivisions 12)
  putStrLn $ "allMultiplications 3 = " ++ show (allMultiplications 3)
  
  putStrLn $ "allOperations 12 = " ++ show (allOperations 12)
  putStrLn $ "allOperationsWithBind 12 = " ++ show (allOperationsWithBind 12)
  
  putStrLn $ "guardExample = " ++ show guardExample

-- Пример 2: Использование монадических функций с List
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Использование монадических функций с List"
  
  -- Использование filterM
  let evenM :: Int -> [] Bool
      evenM x = return (x `mod` 2 == 0)
  putStrLn $ "filterM evenM [1..10] = " ++ show (filterM evenM ([1..10] :: [Int]))
  
  -- Использование foldM
  let safeDiv :: Int -> Int -> [] Int
      safeDiv a b = if b == 0 then [] else [a `div` b]
  putStrLn $ "foldM safeDiv 100 [5, 4, 0, 2] = " ++ show (foldM safeDiv 100 ([5, 4, 0, 2] :: [Int]))
  putStrLn $ "foldM safeDiv 100 [5, 4, 8, 2] = " ++ show (foldM safeDiv 100 ([5, 4, 8, 2] :: [Int]))
  
  -- Использование sequence
  putStrLn $ "sequence [[1, 2], [3, 4]] = " ++ show (sequence ([[1, 2], [3, 4]] :: [[Int]]))
  
  -- Использование mapM
  let duplicate :: Int -> [] Int
      duplicate x = [x, x]
  putStrLn $ "mapM duplicate [1, 2, 3] = " ++ show (mapM duplicate ([1, 2, 3] :: [Int]))

-- Пример 3: Использование IO для ввода-вывода
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Использование IO для ввода-вывода"
  
  putStrLn "Пример функции getNameAndGreet:"
  putStrLn "getNameAndGreet = do"
  putStrLn "  putStrLn \"Как вас зовут?\""
  putStrLn "  name <- getLine"
  putStrLn "  putStrLn $ \"Привет, \" ++ name ++ \"!\""
  
  putStrLn "\nПример функции calculateSum:"
  putStrLn "calculateSum = do"
  putStrLn "  putStrLn \"Сейчас мы вычислим сумму двух чисел.\""
  putStrLn "  x <- getNumber"
  putStrLn "  y <- getNumber"
  putStrLn "  let sum = x + y"
  putStrLn "  putStrLn $ \"Сумма \" ++ show x ++ \" и \" ++ show y ++ \" равна \" ++ show sum"
  
  -- Мы не будем вызывать эти функции здесь, чтобы не прерывать вывод примеров

-- Пример 4: Использование монадических функций с IO
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Использование монадических функций с IO"
  
  -- Использование sequence
  putStrLn "sequence [putStrLn \"Hello\", putStrLn \"World\"] выполнит обе операции последовательно"
  
  -- Использование mapM
  putStrLn "mapM putStrLn [\"Hello\", \"World\"] выполнит putStrLn для каждого элемента списка"
  
  -- Использование forM
  putStrLn "forM [\"Hello\", \"World\"] putStrLn выполнит putStrLn для каждого элемента списка (аналогично mapM)"
  
  -- Использование forever
  putStrLn "forever (putStrLn \"Hello\") будет бесконечно выводить \"Hello\""
  
  -- Использование when
  putStrLn "when True (putStrLn \"Hello\") выполнит putStrLn \"Hello\", а when False (putStrLn \"Hello\") не выполнит ничего"

-- Пример 5: Сравнение List и IO
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Сравнение List и IO"
  
  putStrLn "List:"
  putStrLn "- Представляет недетерминированные вычисления с множеством результатов"
  putStrLn "- Позволяет выразить все возможные результаты вычисления"
  putStrLn "- Используется для поиска решений, перебора вариантов и т.д."
  
  putStrLn "\nIO:"
  putStrLn "- Представляет вычисления с побочными эффектами"
  putStrLn "- Позволяет выполнять операции ввода-вывода в чистом функциональном языке"
  putStrLn "- Используется для взаимодействия с внешним миром"

-- Пример 6: Чистые функции и IO
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Чистые функции и IO"
  
  putStrLn "В Haskell есть четкое разделение между чистыми функциями и функциями с побочными эффектами:"
  
  putStrLn "\nЧистые функции:"
  putStrLn "- Не имеют побочных эффектов"
  putStrLn "- Всегда возвращают одинаковый результат для одинаковых аргументов"
  putStrLn "- Могут быть легко тестированы и оптимизированы"
  putStrLn "- Примеры: allDivisions, allMultiplications, allOperations"
  
  putStrLn "\nФункции с побочными эффектами (IO):"
  putStrLn "- Могут иметь побочные эффекты (ввод-вывод, изменение состояния и т.д.)"
  putStrLn "- Могут возвращать разные результаты для одинаковых аргументов"
  putStrLn "- Требуют особого подхода к тестированию"
  putStrLn "- Примеры: getNameAndGreet, getNumber, calculateSum"
  
  putStrLn "\nМонада IO позволяет четко отделить чистые вычисления от вычислений с побочными эффектами"
  putStrLn "и при этом сохранить чистоту языка."

-- Главная функция
main :: IO ()
main = do
  putStrLn "List и IO монады в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о List и IO монадах:"
  putStrLn "1. List представляет недетерминированные вычисления с множеством результатов"
  putStrLn "2. IO представляет вычисления с побочными эффектами"
  putStrLn "3. Обе монады позволяют писать код в монадическом стиле с использованием do-нотации"
  putStrLn "4. List используется для поиска решений, перебора вариантов и т.д."
  putStrLn "5. IO используется для взаимодействия с внешним миром (ввод-вывод, работа с файлами и т.д.)"
