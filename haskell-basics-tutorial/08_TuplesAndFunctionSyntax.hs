{-
  Кортежи и синтаксис функций в Haskell
  
  В этом файле мы рассмотрим кортежи (tuples) и различные способы
  определения функций в Haskell.
-}

module Main where

-- Часть 1: Кортежи в Haskell

-- Кортеж (tuple) - это упорядоченная коллекция элементов фиксированной длины,
-- которые могут иметь разные типы.

-- Пример 1: Создание кортежей
pair :: (Int, String)
pair = (42, "Hello")

triple :: (Int, String, Bool)
triple = (42, "Hello", True)

-- Кортеж из одного элемента не существует, (x) - это просто x в скобках
-- Кортеж из нуля элементов - это тип (), называемый "unit"
unit :: ()
unit = ()

-- Пример 2: Доступ к элементам кортежа

-- Для пар (кортежей из двух элементов) есть стандартные функции fst и snd
firstElement :: Int
firstElement = fst (42, "Hello")  -- 42

secondElement :: String
secondElement = snd (42, "Hello")  -- "Hello"

-- Для кортежей большей длины нужно использовать сопоставление с образцом
getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x

getSecond :: (a, b, c) -> b
getSecond (_, y, _) = y

getThird :: (a, b, c) -> c
getThird (_, _, z) = z

-- Пример 3: Использование кортежей для возврата нескольких значений из функции

-- Функция, которая возвращает минимальное и максимальное значение из списка
minMax :: [Int] -> (Int, Int)
minMax [] = error "Пустой список"
minMax [x] = (x, x)
minMax (x:xs) =
  let (min, max) = minMax xs
  in (if x < min then x else min, if x > max then x else max)

-- Функция, которая возвращает количество четных, нечетных и нулевых элементов
countEvenOddZero :: [Int] -> (Int, Int, Int)
countEvenOddZero = foldr count (0, 0, 0)
  where
    count 0 (e, o, z) = (e, o, z + 1)
    count x (e, o, z)
      | even x    = (e + 1, o, z)
      | otherwise = (e, o + 1, z)

-- Пример 4: Кортежи в сопоставлении с образцом

-- Функция, которая обрабатывает пары по-разному в зависимости от их содержимого
processPair :: (Int, String) -> String
processPair (0, s) = "Нулевой элемент: " ++ s
processPair (n, "") = "Пустая строка с числом: " ++ show n
processPair (n, s) = "Пара: " ++ show n ++ " и " ++ s

-- Функция, которая обрабатывает список пар
processPairs :: [(Int, String)] -> [String]
processPairs = map processPair

-- Пример 5: Кортежи и функции высшего порядка

-- Функция zip создает список пар из двух списков
zipped :: [(Int, Char)]
zipped = zip [1, 2, 3] ['a', 'b', 'c']  -- [(1,'a'), (2,'b'), (3,'c')]

-- Функция unzip разделяет список пар на два списка
unzipped :: ([Int], [Char])
unzipped = unzip zipped  -- ([1, 2, 3], "abc")

-- Функция zipWith применяет функцию к соответствующим элементам двух списков
zipWithExample :: [Int]
zipWithExample = zipWith (+) [1, 2, 3] [4, 5, 6]  -- [5, 7, 9]

-- Пример 6: Кортежи и типы

-- Тип кортежа - это кортеж типов его элементов
-- (Int, String) - тип пары, где первый элемент имеет тип Int, а второй - String

-- Функция, которая принимает кортеж как аргумент
addPair :: (Int, Int) -> Int
addPair (x, y) = x + y

-- Функция, которая возвращает кортеж
makePair :: a -> b -> (a, b)
makePair x y = (x, y)

-- Функция curry преобразует функцию, принимающую пару, в функцию двух аргументов
curriedAddPair :: Int -> Int -> Int
curriedAddPair = curry addPair

-- Функция uncurry преобразует функцию двух аргументов в функцию, принимающую пару
uncurriedAdd :: (Int, Int) -> Int
uncurriedAdd = uncurry (+)

-- Часть 2: Синтаксис функций в Haskell

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
  putStrLn "Пример 1: Создание кортежей"
  
  putStrLn $ "Пара: " ++ show pair
  putStrLn $ "Тройка: " ++ show triple
  putStrLn $ "Unit: " ++ show unit

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Доступ к элементам кортежа"
  
  putStrLn $ "Первый элемент пары: " ++ show firstElement
  putStrLn $ "Второй элемент пары: " ++ show secondElement
  
  putStrLn $ "Первый элемент тройки: " ++ show (getFirst triple)
  putStrLn $ "Второй элемент тройки: " ++ show (getSecond triple)
  putStrLn $ "Третий элемент тройки: " ++ show (getThird triple)

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Использование кортежей для возврата нескольких значений из функции"
  
  let list = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  putStrLn $ "Список: " ++ show list
  putStrLn $ "Минимум и максимум: " ++ show (minMax list)
  putStrLn $ "Количество четных, нечетных и нулевых элементов: " ++ show (countEvenOddZero list)

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Кортежи в сопоставлении с образцом"
  
  putStrLn $ "processPair (0, \"test\"): " ++ processPair (0, "test")
  putStrLn $ "processPair (42, \"\"): " ++ processPair (42, "")
  putStrLn $ "processPair (42, \"test\"): " ++ processPair (42, "test")
  
  let pairs = [(0, "zero"), (1, ""), (2, "two")]
  putStrLn $ "processPairs " ++ show pairs ++ ": " ++ show (processPairs pairs)

example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Кортежи и функции высшего порядка"
  
  putStrLn $ "zip [1, 2, 3] ['a', 'b', 'c']: " ++ show zipped
  putStrLn $ "unzip " ++ show zipped ++ ": " ++ show unzipped
  putStrLn $ "zipWith (+) [1, 2, 3] [4, 5, 6]: " ++ show zipWithExample

example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Кортежи и типы"
  
  putStrLn $ "addPair (3, 4): " ++ show (addPair (3, 4))
  putStrLn $ "makePair 3 \"hello\": " ++ show (makePair 3 "hello")
  putStrLn $ "curriedAddPair 3 4: " ++ show (curriedAddPair 3 4)
  putStrLn $ "uncurriedAdd (3, 4): " ++ show (uncurriedAdd (3, 4))

example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Определение функций с использованием уравнений и охранных выражений"
  
  putStrLn $ "double 5: " ++ show (double 5)
  putStrLn $ "factorial 5: " ++ show (factorial 5)
  putStrLn $ "absolute (-5): " ++ show (absolute (-5))
  putStrLn $ "fibonacci 10: " ++ show (fibonacci 10)

example8 :: IO ()
example8 = do
  putStrLn "\nПример 8: Определение функций с использованием выражений case и лямбда-выражений"
  
  putStrLn $ "isEmptyList []: " ++ show (isEmptyList ([] :: [Int]))
  putStrLn $ "isEmptyList [1, 2, 3]: " ++ show (isEmptyList [1, 2, 3])
  putStrLn $ "headOrDefault 0 []: " ++ show (headOrDefault 0 ([] :: [Int]))
  putStrLn $ "headOrDefault 0 [1, 2, 3]: " ++ show (headOrDefault 0 [1, 2, 3])
  
  let add5 = makeAdder 5
  putStrLn $ "makeAdder 5 $ 10: " ++ show (add5 10)
  putStrLn $ "squareList [1, 2, 3, 4, 5]: " ++ show (squareList [1, 2, 3, 4, 5])

example9 :: IO ()
example9 = do
  putStrLn "\nПример 9: Определение функций с использованием операторов и секций"
  
  putStrLn $ "3 ^+^ 4: " ++ show (3 ^+^ 4)
  putStrLn $ "addPrefix 3 4: " ++ show (addPrefix 3 4)
  putStrLn $ "3 `div` 2: " ++ show infixExample
  
  putStrLn $ "addFive 10: " ++ show (addFive 10)
  putStrLn $ "multiplyByTwo 10: " ++ show (multiplyByTwo 10)
  putStrLn $ "divideByTwo 10: " ++ show (divideByTwo 10)
  putStrLn $ "isPositive (-5): " ++ show (isPositive (-5))
  putStrLn $ "isPositive 5: " ++ show (isPositive 5)

example10 :: IO ()
example10 = do
  putStrLn "\nПример 10: Определение функций с использованием композиции и точечного стиля"
  
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
  putStrLn "Кортежи и синтаксис функций в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  example8
  example9
  example10
  
  putStrLn "\nКлючевые моменты о кортежах:"
  putStrLn "1. Кортеж - это упорядоченная коллекция элементов фиксированной длины, которые могут иметь разные типы"
  putStrLn "2. Кортежи создаются с помощью круглых скобок и запятых: (1, \"hello\", True)"
  putStrLn "3. Тип кортежа - это кортеж типов его элементов: (Int, String, Bool)"
  putStrLn "4. Для доступа к элементам кортежа можно использовать функции fst и snd (для пар) или сопоставление с образцом"
  putStrLn "5. Кортежи часто используются для возврата нескольких значений из функции"
  putStrLn "6. Функции zip, unzip и zipWith работают с кортежами и списками"
  putStrLn "7. Функции curry и uncurry преобразуют между функциями, принимающими пару, и функциями двух аргументов"
  
  putStrLn "\nКлючевые моменты о синтаксисе функций:"
  putStrLn "1. Функции можно определять с помощью уравнений, охранных выражений, выражений case и лямбда-выражений"
  putStrLn "2. Операторы - это функции, которые можно использовать в инфиксной форме"
  putStrLn "3. Секции - это частично примененные операторы: (+ 5), (* 2), (> 0)"
  putStrLn "4. Композиция функций с оператором (.) позволяет комбинировать функции"
  putStrLn "5. Точечный стиль (point-free style) позволяет определять функции без явного указания аргументов"
  putStrLn "6. Различные стили определения функций могут сделать код более читаемым и выразительным"
  putStrLn "7. Выбор стиля зависит от конкретной задачи и личных предпочтений"
