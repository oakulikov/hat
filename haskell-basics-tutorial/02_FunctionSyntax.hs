{-
  Синтаксис функций в Haskell
  
  В этом файле мы рассмотрим различные способы определения функций в Haskell.
-}

module Main where

-- Синтаксис функций в Haskell

-- Пример 1: Определение функций с использованием уравнений

-- Простая функция с одним уравнением
double :: Int -> Int
double x = x * 2

-- Функция с несколькими уравнениями (сопоставление с образцом)
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Пример 2: Определение функций с использованием охранных выражений (guards)

-- Функция с охранными выражениями
absolute :: Int -> Int
absolute n
  | n < 0     = -n
  | otherwise = n

-- Функция с охранными выражениями и сопоставлением с образцом
fibonacci :: Integer -> Integer
fibonacci n
  | n < 0     = error "Отрицательный аргумент"
  | otherwise = fib n
  where
    fib 0 = 0
    fib 1 = 1
    fib m = fib (m - 1) + fib (m - 2)

-- Пример 3: Определение функций с использованием выражений case

-- Функция с выражением case
isEmptyList :: [a] -> Bool
isEmptyList xs = case xs of
  []    -> True
  (_:_) -> False

-- Функция с вложенными выражениями case
headOrDefault :: a -> [a] -> a
headOrDefault def xs = case xs of
  []    -> def
  (x:_) -> x

-- Пример 4: Определение функций с использованием лямбда-выражений

-- Функция, которая возвращает лямбда-выражение
makeAdder :: Int -> (Int -> Int)
makeAdder n = \x -> n + x

-- Использование лямбда-выражения в функции высшего порядка
squareList :: [Int] -> [Int]
squareList = map (\x -> x * x)

-- Пример 5: Определение функций с использованием операторов в инфиксной форме

-- Определение собственного оператора
(^+^) :: Int -> Int -> Int
x ^+^ y = x^2 + y^2

-- Использование стандартных операторов в префиксной форме
addPrefix :: Int -> Int -> Int
addPrefix = (+)

-- Использование функций в инфиксной форме
infixExample :: Int
infixExample = 3 `div` 2

-- Пример 6: Определение функций с использованием секций

-- Секция - это частично примененный оператор
addFive :: Int -> Int
addFive = (+ 5)

-- Различные секции
multiplyByTwo :: Int -> Int
multiplyByTwo = (* 2)

divideByTwo :: Double -> Double
divideByTwo = (/ 2)

isPositive :: Int -> Bool
isPositive = (> 0)

-- Пример 7: Определение функций с использованием композиции функций

-- Композиция функций с оператором (.)
composed :: Int -> Int
composed = (* 2) . (+ 1)  -- \x -> (x + 1) * 2

-- Композиция нескольких функций
multiComposed :: Int -> Int
multiComposed = (* 2) . (+ 1) . (^ 2) . (+ 3)  -- \x -> ((x + 3)^2 + 1) * 2

-- Композиция с функциями, принимающими несколько аргументов
composedMultiArg :: Int -> Int -> Int
composedMultiArg x y = (+ 1) ((x * y))  -- \x y -> (x * y) + 1

-- Пример 8: Определение функций с использованием точечного стиля (point-free style)

-- Функция в обычном стиле
sumList :: [Int] -> Int
sumList xs = sum xs

-- Та же функция в точечном стиле
sumListPointFree :: [Int] -> Int
sumListPointFree = sum

-- Более сложный пример
filterAndSum :: [Int] -> Int
filterAndSum xs = sum (filter even xs)

-- Та же функция в точечном стиле
filterAndSumPointFree :: [Int] -> Int
filterAndSumPointFree = sum . filter even

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Определение функций с использованием уравнений и охранных выражений"
  
  putStrLn $ "double 5: " ++ show (double 5)
  putStrLn $ "factorial 5: " ++ show (factorial 5)
  putStrLn $ "absolute (-5): " ++ show (absolute (-5))
  putStrLn $ "fibonacci 10: " ++ show (fibonacci 10)

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Определение функций с использованием выражений case и лямбда-выражений"
  
  putStrLn $ "isEmptyList []: " ++ show (isEmptyList ([] :: [Int]))
  putStrLn $ "isEmptyList [1, 2, 3]: " ++ show (isEmptyList [1, 2, 3])
  putStrLn $ "headOrDefault 0 []: " ++ show (headOrDefault 0 ([] :: [Int]))
  putStrLn $ "headOrDefault 0 [1, 2, 3]: " ++ show (headOrDefault 0 [1, 2, 3])
  
  let add5 = makeAdder 5
  putStrLn $ "makeAdder 5 $ 10: " ++ show (add5 10)
  putStrLn $ "squareList [1, 2, 3, 4, 5]: " ++ show (squareList [1, 2, 3, 4, 5])

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Определение функций с использованием операторов и секций"
  
  putStrLn $ "3 ^+^ 4: " ++ show (3 ^+^ 4)
  putStrLn $ "addPrefix 3 4: " ++ show (addPrefix 3 4)
  putStrLn $ "3 `div` 2: " ++ show infixExample
  
  putStrLn $ "addFive 10: " ++ show (addFive 10)
  putStrLn $ "multiplyByTwo 10: " ++ show (multiplyByTwo 10)
  putStrLn $ "divideByTwo 10: " ++ show (divideByTwo 10)
  putStrLn $ "isPositive (-5): " ++ show (isPositive (-5))
  putStrLn $ "isPositive 5: " ++ show (isPositive 5)

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Определение функций с использованием композиции и точечного стиля"
  
  putStrLn $ "composed 5: " ++ show (composed 5)
  putStrLn $ "multiComposed 5: " ++ show (multiComposed 5)
  putStrLn $ "composedMultiArg 3 4: " ++ show (composedMultiArg 3 4)
  
  putStrLn $ "sumList [1, 2, 3, 4, 5]: " ++ show (sumList [1, 2, 3, 4, 5])
  putStrLn $ "sumListPointFree [1, 2, 3, 4, 5]: " ++ show (sumListPointFree [1, 2, 3, 4, 5])
  putStrLn $ "filterAndSum [1, 2, 3, 4, 5]: " ++ show (filterAndSum [1, 2, 3, 4, 5])
  putStrLn $ "filterAndSumPointFree [1, 2, 3, 4, 5]: " ++ show (filterAndSumPointFree [1, 2, 3, 4, 5])

-- Главная функция
main :: IO ()
main = do
  putStrLn "Синтаксис функций в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о синтаксисе функций:"
  putStrLn "1. Функции можно определять с помощью уравнений, охранных выражений, выражений case и лямбда-выражений"
  putStrLn "2. Операторы - это функции, которые можно использовать в инфиксной форме"
  putStrLn "3. Секции - это частично примененные операторы: (+ 5), (* 2), (> 0)"
  putStrLn "4. Композиция функций с оператором (.) позволяет комбинировать функции"
  putStrLn "5. Точечный стиль (point-free style) позволяет определять функции без явного указания аргументов"
  putStrLn "6. Различные стили определения функций могут сделать код более читаемым и выразительным"
  putStrLn "7. Выбор стиля зависит от конкретной задачи и личных предпочтений"
