{-
  Списки и рекурсия в Haskell
  
  В этом файле мы рассмотрим работу со списками и рекурсивные функции,
  которые являются фундаментальными концепциями в функциональном программировании.
-}

module Main where

import Data.Char (toUpper)

-- Основы списков в Haskell

-- Создание списков
emptyList :: [Int]
emptyList = []

numbers :: [Int]
numbers = [1, 2, 3, 4, 5]

characters :: [Char]
characters = ['a', 'b', 'c', 'd', 'e']

-- Строки - это списки символов
greeting :: String
greeting = "Hello, Haskell!"

-- Создание списков с помощью диапазонов
range1 :: [Int]
range1 = [1..10]  -- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

range2 :: [Int]
range2 = [2, 4..10]  -- [2, 4, 6, 8, 10]

range3 :: [Char]
range3 = ['a'..'e']  -- ['a', 'b', 'c', 'd', 'e']

-- Создание списков с помощью списковых включений (list comprehensions)
squares :: [Int]
squares = [x^2 | x <- [1..10]]  -- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

evenSquares :: [Int]
evenSquares = [x^2 | x <- [1..10], even x]  -- [4, 16, 36, 64, 100]

pairs :: [(Int, Int)]
pairs = [(x, y) | x <- [1..3], y <- [1..3]]  -- [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]

-- Операции со списками

-- Конкатенация списков
concatenated :: [Int]
concatenated = [1, 2, 3] ++ [4, 5, 6]  -- [1, 2, 3, 4, 5, 6]

-- Добавление элемента в начало списка (cons)
consed :: [Int]
consed = 0 : [1, 2, 3]  -- [0, 1, 2, 3]

-- Доступ к элементам списка по индексу
thirdElement :: Int
thirdElement = [10, 20, 30, 40, 50] !! 2  -- 30 (индексы начинаются с 0)

-- Базовые функции для работы со списками

-- head - возвращает первый элемент списка
firstElement :: Int
firstElement = head [10, 20, 30, 40, 50]  -- 10

-- tail - возвращает список без первого элемента
restOfList :: [Int]
restOfList = tail [10, 20, 30, 40, 50]  -- [20, 30, 40, 50]

-- init - возвращает список без последнего элемента
allButLast :: [Int]
allButLast = init [10, 20, 30, 40, 50]  -- [10, 20, 30, 40]

-- last - возвращает последний элемент списка
lastElement :: Int
lastElement = last [10, 20, 30, 40, 50]  -- 50

-- length - возвращает длину списка
listLength :: Int
listLength = length [10, 20, 30, 40, 50]  -- 5

-- null - проверяет, пуст ли список
isEmpty1 :: Bool
isEmpty1 = null []  -- True

isEmpty2 :: Bool
isEmpty2 = null [1, 2, 3]  -- False

-- reverse - переворачивает список
reversed :: [Int]
reversed = reverse [1, 2, 3, 4, 5]  -- [5, 4, 3, 2, 1]

-- take - берет n первых элементов списка
firstThree :: [Int]
firstThree = take 3 [1, 2, 3, 4, 5]  -- [1, 2, 3]

-- drop - отбрасывает n первых элементов списка
withoutFirstThree :: [Int]
withoutFirstThree = drop 3 [1, 2, 3, 4, 5]  -- [4, 5]

-- maximum - возвращает максимальный элемент списка
maxElement :: Int
maxElement = maximum [3, 1, 4, 1, 5, 9, 2, 6]  -- 9

-- minimum - возвращает минимальный элемент списка
minElement :: Int
minElement = minimum [3, 1, 4, 1, 5, 9, 2, 6]  -- 1

-- sum - суммирует элементы списка
sumOfList :: Int
sumOfList = sum [1, 2, 3, 4, 5]  -- 15

-- product - перемножает элементы списка
productOfList :: Int
productOfList = product [1, 2, 3, 4, 5]  -- 120

-- elem - проверяет, содержится ли элемент в списке
contains3 :: Bool
contains3 = 3 `elem` [1, 2, 3, 4, 5]  -- True

contains6 :: Bool
contains6 = 6 `elem` [1, 2, 3, 4, 5]  -- False

-- Рекурсивные функции для работы со списками

-- Функция, которая вычисляет длину списка
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Функция, которая суммирует элементы списка
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- Функция, которая перемножает элементы списка
myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

-- Функция, которая проверяет, содержится ли элемент в списке
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs)
  | e == x    = True
  | otherwise = myElem e xs

-- Функция, которая объединяет два списка
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- Функция, которая переворачивает список
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Функция, которая применяет функцию к каждому элементу списка (map)
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Функция, которая фильтрует элементы списка по предикату (filter)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise = myFilter p xs

-- Функция, которая объединяет элементы списка с помощью функции (foldr)
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

-- Функция, которая объединяет элементы списка с помощью функции (foldl)
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

-- Примеры рекурсивных функций для работы с числами

-- Функция, которая вычисляет факториал числа
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Функция, которая вычисляет n-е число Фибоначчи
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Функция, которая вычисляет наибольший общий делитель (алгоритм Евклида)
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Функция, которая проверяет, является ли число простым
isPrime :: Integer -> Bool
isPrime n
  | n <= 1    = False
  | n <= 3    = True
  | even n    = False
  | otherwise = all (\x -> n `mod` x /= 0) [3, 5..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

-- Функция, которая возвращает список простых чисел до n
primesUpTo :: Integer -> [Integer]
primesUpTo n = [x | x <- [2..n], isPrime x]

-- Примеры рекурсивных функций для работы со списками

-- Функция, которая возвращает список четных чисел
evens :: [Integer] -> [Integer]
evens [] = []
evens (x:xs)
  | even x    = x : evens xs
  | otherwise = evens xs

-- Функция, которая удваивает каждый элемент списка
doubleList :: Num a => [a] -> [a]
doubleList [] = []
doubleList (x:xs) = (2 * x) : doubleList xs

-- Функция, которая объединяет соседние элементы списка попарно
pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp [_] = []
pairUp (x:y:xs) = (x, y) : pairUp xs

-- Функция, которая разбивает список на подсписки заданной длины
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs
  | n <= 0    = []
  | otherwise = take n xs : chunk n (drop n xs)

-- Функция, которая возвращает все возможные подсписки списка
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = sublists xs ++ map (x:) (sublists xs)

-- Функция, которая возвращает все возможные перестановки списка
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:zs | y <- xs, zs <- permutations (filter (/= y) xs)]

-- Функция, которая сортирует список (сортировка вставками)
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insert y zs

-- Функция, которая сортирует список (быстрая сортировка)
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
  let smaller = quickSort [y | y <- xs, y <= x]
      larger  = quickSort [y | y <- xs, y > x]
  in smaller ++ [x] ++ larger

-- Функция, которая сортирует список (сортировка слиянием)
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (ys, zs) = splitAt (length xs `div` 2) xs
  in merge (mergeSort ys) (mergeSort zs)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- Пример 1: Основы списков
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Основы списков"
  
  putStrLn $ "Пустой список: " ++ show emptyList
  putStrLn $ "Список чисел: " ++ show numbers
  putStrLn $ "Список символов: " ++ show characters
  putStrLn $ "Строка (список символов): " ++ greeting
  
  putStrLn $ "Диапазон [1..10]: " ++ show range1
  putStrLn $ "Диапазон [2, 4..10]: " ++ show range2
  putStrLn $ "Диапазон ['a'..'e']: " ++ show range3
  
  putStrLn $ "Квадраты чисел от 1 до 10: " ++ show squares
  putStrLn $ "Квадраты четных чисел от 1 до 10: " ++ show evenSquares
  putStrLn $ "Пары чисел от 1 до 3: " ++ show pairs

-- Пример 2: Операции со списками
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Операции со списками"
  
  putStrLn $ "Конкатенация [1, 2, 3] ++ [4, 5, 6]: " ++ show concatenated
  putStrLn $ "Добавление элемента 0 : [1, 2, 3]: " ++ show consed
  putStrLn $ "Третий элемент [10, 20, 30, 40, 50] !! 2: " ++ show thirdElement
  
  putStrLn $ "Первый элемент (head): " ++ show firstElement
  putStrLn $ "Список без первого элемента (tail): " ++ show restOfList
  putStrLn $ "Список без последнего элемента (init): " ++ show allButLast
  putStrLn $ "Последний элемент (last): " ++ show lastElement
  putStrLn $ "Длина списка (length): " ++ show listLength
  
  putStrLn $ "Пустой список? (null []): " ++ show isEmpty1
  putStrLn $ "Пустой список? (null [1, 2, 3]): " ++ show isEmpty2
  putStrLn $ "Перевернутый список (reverse): " ++ show reversed
  putStrLn $ "Первые три элемента (take 3): " ++ show firstThree
  putStrLn $ "Без первых трех элементов (drop 3): " ++ show withoutFirstThree
  
  putStrLn $ "Максимальный элемент (maximum): " ++ show maxElement
  putStrLn $ "Минимальный элемент (minimum): " ++ show minElement
  putStrLn $ "Сумма элементов (sum): " ++ show sumOfList
  putStrLn $ "Произведение элементов (product): " ++ show productOfList
  
  putStrLn $ "Содержит 3? (3 `elem` [1, 2, 3, 4, 5]): " ++ show contains3
  putStrLn $ "Содержит 6? (6 `elem` [1, 2, 3, 4, 5]): " ++ show contains6

-- Пример 3: Рекурсивные функции для работы со списками
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Рекурсивные функции для работы со списками"
  
  let list = [1, 2, 3, 4, 5]
  
  putStrLn $ "Список: " ++ show list
  putStrLn $ "Длина списка (myLength): " ++ show (myLength list)
  putStrLn $ "Сумма элементов (mySum): " ++ show (mySum list)
  putStrLn $ "Произведение элементов (myProduct): " ++ show (myProduct list)
  putStrLn $ "Содержит 3? (myElem 3): " ++ show (myElem 3 list)
  putStrLn $ "Содержит 6? (myElem 6): " ++ show (myElem 6 list)
  
  putStrLn $ "Объединение [1, 2] и [3, 4, 5] (myAppend): " ++ show (myAppend [1, 2] [3, 4, 5])
  putStrLn $ "Перевернутый список (myReverse): " ++ show (myReverse list)
  
  putStrLn $ "Удвоение элементов (myMap (* 2)): " ++ show (myMap (* 2) list)
  putStrLn $ "Четные элементы (myFilter even): " ++ show (myFilter even list)
  
  putStrLn $ "Сумма элементов (myFoldr (+) 0): " ++ show (myFoldr (+) 0 list)
  putStrLn $ "Произведение элементов (myFoldr (*) 1): " ++ show (myFoldr (*) 1 list)
  putStrLn $ "Сумма элементов (myFoldl (+) 0): " ++ show (myFoldl (+) 0 list)
  putStrLn $ "Произведение элементов (myFoldl (*) 1): " ++ show (myFoldl (*) 1 list)

-- Пример 4: Рекурсивные функции для работы с числами
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Рекурсивные функции для работы с числами"
  
  putStrLn $ "Факториал 5 (factorial): " ++ show (factorial 5)
  putStrLn $ "Число Фибоначчи 10 (fibonacci): " ++ show (fibonacci 10)
  putStrLn $ "НОД 48 и 18 (gcd'): " ++ show (gcd' 48 18)
  
  putStrLn $ "Простое число 7? (isPrime): " ++ show (isPrime 7)
  putStrLn $ "Простое число 10? (isPrime): " ++ show (isPrime 10)
  putStrLn $ "Простые числа до 20 (primesUpTo): " ++ show (primesUpTo 20)

-- Пример 5: Дополнительные рекурсивные функции для работы со списками
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Дополнительные рекурсивные функции для работы со списками"
  
  let list = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  putStrLn $ "Список: " ++ show list
  putStrLn $ "Четные числа (evens): " ++ show (evens list)
  putStrLn $ "Удвоение элементов (doubleList): " ++ show (doubleList list)
  putStrLn $ "Объединение попарно (pairUp): " ++ show (pairUp list)
  putStrLn $ "Разбиение на группы по 3 (chunk 3): " ++ show (chunk 3 list)
  
  let smallList = [1, 2, 3]
  putStrLn $ "Все подсписки [1, 2, 3] (sublists): " ++ show (sublists smallList)
  putStrLn $ "Все перестановки [1, 2, 3] (permutations): " ++ show (permutations smallList)

-- Пример 6: Алгоритмы сортировки
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Алгоритмы сортировки"
  
  let unsorted = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
  
  putStrLn $ "Несортированный список: " ++ show unsorted
  putStrLn $ "Сортировка вставками (insertionSort): " ++ show (insertionSort unsorted)
  putStrLn $ "Быстрая сортировка (quickSort): " ++ show (quickSort unsorted)
  putStrLn $ "Сортировка слиянием (mergeSort): " ++ show (mergeSort unsorted)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Списки и рекурсия в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о списках и рекурсии:"
  putStrLn "1. Списки - это одна из основных структур данных в Haskell"
  putStrLn "2. Списки могут содержать элементы одного типа"
  putStrLn "3. Строки - это списки символов (String = [Char])"
  putStrLn "4. Списки можно создавать с помощью диапазонов и списковых включений"
  putStrLn "5. Основные операции со списками: конкатенация (++), добавление элемента (:), доступ по индексу (!!)"
  putStrLn "6. Стандартная библиотека предоставляет множество функций для работы со списками"
  putStrLn "7. Рекурсия - это основной механизм для работы со списками в функциональном программировании"
  putStrLn "8. Рекурсивные функции определяются через базовый случай и рекурсивный случай"
  putStrLn "9. Функции высшего порядка, такие как map, filter и fold, позволяют абстрагировать рекурсивные паттерны"
  putStrLn "10. Рекурсия может использоваться для реализации различных алгоритмов, включая сортировку и поиск"
