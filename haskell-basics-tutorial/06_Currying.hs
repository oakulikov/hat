{-
  Каррирование в Haskell
  
  В этом файле мы рассмотрим каррирование (currying) и частичное применение функций,
  которые являются фундаментальными концепциями в функциональном программировании.
-}

module Main where

import Data.List (sortBy)
import Data.Char (toUpper)

-- Каррирование - это преобразование функции от нескольких аргументов
-- в последовательность функций, каждая из которых принимает один аргумент.
-- В Haskell все функции по умолчанию каррированы.

-- Пример 1: Каррированные функции

-- Функция двух аргументов
add :: Int -> Int -> Int
add x y = x + y

-- Эквивалентная запись с явным каррированием
addCurried :: Int -> (Int -> Int)
addCurried x = \y -> x + y

-- Функция трех аргументов
multiply3 :: Int -> Int -> Int -> Int
multiply3 x y z = x * y * z

-- Эквивалентная запись с явным каррированием
multiply3Curried :: Int -> (Int -> (Int -> Int))
multiply3Curried x = \y -> \z -> x * y * z

-- Пример 2: Частичное применение функций

-- Частичное применение add
add5 :: Int -> Int
add5 = add 5

-- Частичное применение multiply3
multiply3By2 :: Int -> Int -> Int
multiply3By2 = multiply3 2

-- Частичное применение multiply3 с двумя аргументами
multiply3By2And3 :: Int -> Int
multiply3By2And3 = multiply3 2 3

-- Пример 3: Частичное применение операторов

-- Оператор сложения
(+++) :: Int -> Int -> Int
(+++) = (+)

-- Частичное применение оператора сложения
add10 :: Int -> Int
add10 = (+) 10

-- Оператор умножения
(***) :: Int -> Int -> Int
(***) = (*)

-- Частичное применение оператора умножения
multiplyBy7 :: Int -> Int
multiplyBy7 = (*) 7

-- Оператор сравнения
(>?>) :: Ord a => a -> a -> Bool
(>?>) = (>)

-- Частичное применение оператора сравнения
greaterThan100 :: Int -> Bool
greaterThan100 = (>) 100

-- Пример 4: Частичное применение функций высших порядков

-- Частичное применение map
doubleAll :: [Int] -> [Int]
doubleAll = map (* 2)

-- Частичное применение filter
onlyEven :: [Int] -> [Int]
onlyEven = filter even

-- Частичное применение foldl
sumList :: [Int] -> Int
sumList = foldl (+) 0

-- Частичное применение zipWith
addLists :: [Int] -> [Int] -> [Int]
addLists = zipWith (+)

-- Пример 5: Каррирование и композиция функций

-- Композиция функций
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Частичное применение compose
doubleAndIncrement :: Int -> Int
doubleAndIncrement = compose (+ 1) (* 2)

-- Оператор композиции функций
(>>>) :: (a -> b) -> (b -> c) -> a -> c
f >>> g = \x -> g (f x)

-- Частичное применение оператора композиции
incrementAndDouble :: Int -> Int
incrementAndDouble = (+ 1) >>> (* 2)

-- Пример 6: Каррирование и секции операторов

-- Секции операторов - это частичное применение инфиксных операторов
-- (+ 1)   эквивалентно   \x -> x + 1
-- (1 +)   эквивалентно   \x -> 1 + x

-- Секция оператора сложения
increment :: Int -> Int
increment = (+ 1)

-- Секция оператора умножения
double :: Int -> Int
double = (* 2)

-- Секция оператора вычитания
subtractFrom10 :: Int -> Int
subtractFrom10 = (10 -)

-- Секция оператора деления
divideBy2 :: Double -> Double
divideBy2 = (/ 2)

-- Пример 7: Каррирование и некаррированные функции

-- Некаррированная функция, принимающая пару
addUncurried :: (Int, Int) -> Int
addUncurried (x, y) = x + y

-- Преобразование некаррированной функции в каррированную
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

-- Использование curry'
addCurried' :: Int -> Int -> Int
addCurried' = curry' addUncurried

-- Преобразование каррированной функции в некаррированную
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

-- Использование uncurry'
addUncurried' :: (Int, Int) -> Int
addUncurried' = uncurry' add

-- Пример 8: Практическое применение каррирования

-- Функция для фильтрации списка по предикату
filterBy :: (a -> Bool) -> [a] -> [a]
filterBy p = filter p

-- Частичное применение filterBy
greaterThan5 :: [Int] -> [Int]
greaterThan5 = filterBy (> 5)

-- Функция для сортировки списка по ключу
sortByKey :: Ord b => (a -> b) -> [a] -> [a]
sortByKey f = sortBy (\x y -> compare (f x) (f y))

-- Частичное применение sortByKey
sortByLength :: [String] -> [String]
sortByLength = sortByKey length

-- Функция для преобразования списка
transformList :: (a -> b) -> [a] -> [b]
transformList f = map f

-- Частичное применение transformList
toUpperList :: [String] -> [String]
toUpperList = transformList (map toUpper)

-- Пример 1: Каррированные функции
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Каррированные функции"
  
  putStrLn $ "add 3 4: " ++ show (add 3 4)
  putStrLn $ "addCurried 3 4: " ++ show (addCurried 3 4)
  
  putStrLn $ "multiply3 2 3 4: " ++ show (multiply3 2 3 4)
  putStrLn $ "multiply3Curried 2 3 4: " ++ show (multiply3Curried 2 3 4)
  
  putStrLn $ "Тип add: Int -> Int -> Int"
  putStrLn $ "Тип addCurried: Int -> (Int -> Int)"
  putStrLn $ "Эти типы эквивалентны в Haskell"

-- Пример 2: Частичное применение функций
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Частичное применение функций"
  
  putStrLn $ "add5 10: " ++ show (add5 10)
  putStrLn $ "add 5 10: " ++ show (add 5 10)
  
  putStrLn $ "multiply3By2 3 4: " ++ show (multiply3By2 3 4)
  putStrLn $ "multiply3 2 3 4: " ++ show (multiply3 2 3 4)
  
  putStrLn $ "multiply3By2And3 4: " ++ show (multiply3By2And3 4)
  putStrLn $ "multiply3 2 3 4: " ++ show (multiply3 2 3 4)

-- Пример 3: Частичное применение операторов
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Частичное применение операторов"
  
  putStrLn $ "3 +++ 4: " ++ show (3 +++ 4)
  putStrLn $ "add10 5: " ++ show (add10 5)
  
  putStrLn $ "3 *** 4: " ++ show (3 *** 4)
  putStrLn $ "multiplyBy7 6: " ++ show (multiplyBy7 6)
  
  putStrLn $ "5 >?> 3: " ++ show (5 >?> 3)
  putStrLn $ "greaterThan100 50: " ++ show (greaterThan100 50)
  putStrLn $ "greaterThan100 150: " ++ show (greaterThan100 150)

-- Пример 4: Частичное применение функций высших порядков
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Частичное применение функций высших порядков"
  
  putStrLn $ "doubleAll [1, 2, 3]: " ++ show (doubleAll [1, 2, 3])
  putStrLn $ "map (* 2) [1, 2, 3]: " ++ show (map (* 2) [1, 2, 3])
  
  putStrLn $ "onlyEven [1, 2, 3, 4, 5, 6]: " ++ show (onlyEven [1, 2, 3, 4, 5, 6])
  putStrLn $ "filter even [1, 2, 3, 4, 5, 6]: " ++ show (filter even [1, 2, 3, 4, 5, 6])
  
  putStrLn $ "sumList [1, 2, 3, 4, 5]: " ++ show (sumList [1, 2, 3, 4, 5])
  putStrLn $ "foldl (+) 0 [1, 2, 3, 4, 5]: " ++ show (foldl (+) 0 [1, 2, 3, 4, 5])
  
  putStrLn $ "addLists [1, 2, 3] [4, 5, 6]: " ++ show (addLists [1, 2, 3] [4, 5, 6])
  putStrLn $ "zipWith (+) [1, 2, 3] [4, 5, 6]: " ++ show (zipWith (+) [1, 2, 3] [4, 5, 6])

-- Пример 5: Каррирование и композиция функций
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Каррирование и композиция функций"
  
  putStrLn $ "doubleAndIncrement 5: " ++ show (doubleAndIncrement 5)
  putStrLn $ "compose (+ 1) (* 2) 5: " ++ show (compose (+ 1) (* 2) 5)
  
  putStrLn $ "incrementAndDouble 5: " ++ show (incrementAndDouble 5)
  putStrLn $ "((+ 1) >>> (* 2)) 5: " ++ show (((+ 1) >>> (* 2)) 5)

-- Пример 6: Каррирование и секции операторов
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Каррирование и секции операторов"
  
  putStrLn $ "increment 5: " ++ show (increment 5)
  putStrLn $ "(+ 1) 5: " ++ show ((+ 1) 5)
  
  putStrLn $ "double 5: " ++ show (double 5)
  putStrLn $ "(* 2) 5: " ++ show ((* 2) 5)
  
  putStrLn $ "subtractFrom10 3: " ++ show (subtractFrom10 3)
  putStrLn $ "(10 -) 3: " ++ show ((10 -) 3)
  
  putStrLn $ "divideBy2 10: " ++ show (divideBy2 10)
  putStrLn $ "(/ 2) 10: " ++ show ((/ 2) 10)

-- Пример 7: Каррирование и некаррированные функции
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Каррирование и некаррированные функции"
  
  putStrLn $ "addUncurried (3, 4): " ++ show (addUncurried (3, 4))
  putStrLn $ "addCurried' 3 4: " ++ show (addCurried' 3 4)
  
  putStrLn $ "addUncurried' (3, 4): " ++ show (addUncurried' (3, 4))
  putStrLn $ "add 3 4: " ++ show (add 3 4)
  
  putStrLn $ "curry' addUncurried 3 4: " ++ show (curry' addUncurried 3 4)
  putStrLn $ "uncurry' add (3, 4): " ++ show (uncurry' add (3, 4))

-- Пример 8: Практическое применение каррирования
example8 :: IO ()
example8 = do
  putStrLn "\nПример 8: Практическое применение каррирования"
  
  putStrLn $ "greaterThan5 [1, 5, 10, 3, 7]: " ++ show (greaterThan5 [1, 5, 10, 3, 7])
  putStrLn $ "filterBy (> 5) [1, 5, 10, 3, 7]: " ++ show (filterBy (> 5) [1, 5, 10, 3, 7])
  
  let strings = ["hello", "a", "world", "haskell", "hi"]
  putStrLn $ "sortByLength " ++ show strings ++ ": " ++ show (sortByLength strings)
  putStrLn $ "sortByKey length " ++ show strings ++ ": " ++ show (sortByKey length strings)
  
  putStrLn $ "toUpperList [\"hello\", \"world\"]: " ++ show (toUpperList ["hello", "world"])
  putStrLn $ "transformList (map toUpper) [\"hello\", \"world\"]: " ++ show (transformList (map toUpper) ["hello", "world"])

-- Главная функция
main :: IO ()
main = do
  putStrLn "Каррирование в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  example8
  
  putStrLn "\nКлючевые моменты о каррировании:"
  putStrLn "1. Каррирование - это преобразование функции от нескольких аргументов в последовательность функций от одного аргумента"
  putStrLn "2. В Haskell все функции по умолчанию каррированы"
  putStrLn "3. Частичное применение - это применение функции к меньшему числу аргументов, чем она ожидает"
  putStrLn "4. Частичное применение возвращает новую функцию, которая ожидает оставшиеся аргументы"
  putStrLn "5. Секции операторов - это частичное применение инфиксных операторов"
  putStrLn "6. Каррирование и частичное применение позволяют создавать новые функции из существующих"
  putStrLn "7. Каррирование делает код более модульным и выразительным"
  putStrLn "8. Каррирование и частичное применение широко используются в функциональном программировании"
