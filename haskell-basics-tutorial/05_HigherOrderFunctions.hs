{-
  Функции высших порядков в Haskell
  
  В этом файле мы рассмотрим функции высших порядков,
  которые принимают другие функции в качестве аргументов
  или возвращают функции в качестве результатов.
-}

module Main where

import Data.Char (toUpper, toLower)
import Data.List (sort, sortBy)

-- Функции высших порядков - это функции, которые:
-- 1. Принимают функции в качестве аргументов
-- 2. Возвращают функции в качестве результатов
-- 3. Или и то, и другое

-- Пример 1: Функции, принимающие функции в качестве аргументов

-- map - применяет функцию к каждому элементу списка
mapExample1 :: [Int]
mapExample1 = map (* 2) [1, 2, 3, 4, 5]  -- [2, 4, 6, 8, 10]

mapExample2 :: [String]
mapExample2 = map reverse ["hello", "world", "haskell"]  -- ["olleh", "dlrow", "lleksah"]

-- filter - отбирает элементы списка, удовлетворяющие предикату
filterExample1 :: [Int]
filterExample1 = filter even [1, 2, 3, 4, 5, 6]  -- [2, 4, 6]

filterExample2 :: [String]
filterExample2 = filter ((> 3) . length) ["a", "ab", "abc", "abcd", "abcde"]  -- ["abcd", "abcde"]

-- foldl - свертка списка слева
foldlExample1 :: Int
foldlExample1 = foldl (+) 0 [1, 2, 3, 4, 5]  -- 15

foldlExample2 :: String
foldlExample2 = foldl (\acc x -> acc ++ show x) "" [1, 2, 3, 4, 5]  -- "12345"

-- foldr - свертка списка справа
foldrExample1 :: Int
foldrExample1 = foldr (+) 0 [1, 2, 3, 4, 5]  -- 15

foldrExample2 :: [Int]
foldrExample2 = foldr (:) [] [1, 2, 3, 4, 5]  -- [1, 2, 3, 4, 5]

-- zipWith - применяет функцию к соответствующим элементам двух списков
zipWithExample1 :: [Int]
zipWithExample1 = zipWith (+) [1, 2, 3] [4, 5, 6]  -- [5, 7, 9]

zipWithExample2 :: [String]
zipWithExample2 = zipWith (\x y -> show x ++ "-" ++ y) [1, 2, 3] ["a", "b", "c"]  -- ["1-a", "2-b", "3-c"]

-- sortBy - сортирует список с помощью функции сравнения
sortByExample1 :: [Int]
sortByExample1 = sortBy (\x y -> compare y x) [3, 1, 4, 1, 5, 9, 2, 6]  -- [9, 6, 5, 4, 3, 2, 1, 1]

sortByExample2 :: [(String, Int)]
sortByExample2 = sortBy (\(_, x) (_, y) -> compare x y) [("a", 3), ("b", 1), ("c", 2)]  -- [("b", 1), ("c", 2), ("a", 3)]

-- Пример 2: Функции, возвращающие функции

-- Частичное применение функций
add :: Int -> Int -> Int
add x y = x + y

-- Частичное применение add
add5 :: Int -> Int
add5 = add 5

-- Функция, возвращающая функцию
makeAdder :: Int -> (Int -> Int)
makeAdder n = \x -> n + x

-- Использование makeAdder
add10 :: Int -> Int
add10 = makeAdder 10

-- Функция, возвращающая функцию с несколькими аргументами
makeMultiplier :: Int -> (Int -> Int -> Int)
makeMultiplier n = \x y -> n * x * y

-- Использование makeMultiplier
double :: Int -> Int -> Int
double = makeMultiplier 2

-- Пример 3: Композиция функций

-- Оператор композиции функций (.)
-- (f . g) x = f (g x)
compositionExample1 :: [Int]
compositionExample1 = map ((*2) . (+1)) [1, 2, 3, 4, 5]  -- [4, 6, 8, 10, 12]

compositionExample2 :: [String]
compositionExample2 = map (reverse . map toUpper) ["hello", "world"]  -- ["OLLEH", "DLROW"]

-- Функция, которая применяет список функций к значению
applyAll :: [a -> a] -> a -> a
applyAll [] x = x
applyAll (f:fs) x = applyAll fs (f x)

-- Использование applyAll
applyAllExample :: Int
applyAllExample = applyAll [(+1), (*2), (^2)] 3  -- (3+1)*2^2 = 16

-- Пример 4: Функции высших порядков для работы с функциями

-- flip - меняет местами аргументы функции
flipExample1 :: [String]
flipExample1 = map (flip (++) "!") ["hello", "world"]  -- ["hello!", "world!"]

flipExample2 :: [(Int, Int)]
flipExample2 = map (flip (,) 1) [1, 2, 3]  -- [(1,1), (1,2), (1,3)]

-- curry - преобразует функцию, принимающую пару, в функцию двух аргументов
curryExample :: Int
curryExample = curry (\(x, y) -> x + y) 3 4  -- 7

-- uncurry - преобразует функцию двух аргументов в функцию, принимающую пару
uncurryExample :: Int
uncurryExample = uncurry (+) (3, 4)  -- 7

-- Пример 5: Реализация стандартных функций высших порядков

-- Реализация map
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- Реализация filter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise = myFilter p xs

-- Реализация foldl
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

-- Реализация foldr
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- Реализация zipWith
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

-- Пример 6: Функции высших порядков для обработки данных

-- Функция, которая группирует элементы списка по результату применения функции
groupBy :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupBy f xs = foldr addToGroup [] xs
  where
    addToGroup x groups =
      let key = f x
          (matching, others) = partition (\(k, _) -> k == key) groups
      in case matching of
        [] -> (key, [x]) : others
        [(k, vs)] -> (k, x:vs) : others
        _ -> error "Impossible case in groupBy"
    
    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition p xs = (filter p xs, filter (not . p) xs)

-- Функция, которая подсчитывает количество элементов, удовлетворяющих предикату
countIf :: (a -> Bool) -> [a] -> Int
countIf p = length . filter p

-- Функция, которая находит первый элемент, удовлетворяющий предикату
findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst p (x:xs)
  | p x       = Just x
  | otherwise = findFirst p xs

-- Функция, которая применяет функцию к элементу, если он удовлетворяет предикату
mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf p f = map (\x -> if p x then f x else x)

-- Пример 7: Функции высших порядков для комбинаторных задач

-- Функция, которая возвращает все возможные пары элементов из двух списков
allPairs :: [a] -> [b] -> [(a, b)]
allPairs xs ys = [(x, y) | x <- xs, y <- ys]

-- Функция, которая возвращает все возможные комбинации элементов списка по n
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- Функция, которая возвращает все возможные перестановки списка
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:zs | y <- xs, zs <- permutations (filter (/= y) xs)]

-- Пример 1: Функции, принимающие функции в качестве аргументов
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Функции, принимающие функции в качестве аргументов"
  
  putStrLn $ "map (* 2) [1, 2, 3, 4, 5]: " ++ show mapExample1
  putStrLn $ "map reverse [\"hello\", \"world\", \"haskell\"]: " ++ show mapExample2
  
  putStrLn $ "filter even [1, 2, 3, 4, 5, 6]: " ++ show filterExample1
  putStrLn $ "filter ((> 3) . length) [\"a\", \"ab\", \"abc\", \"abcd\", \"abcde\"]: " ++ show filterExample2
  
  putStrLn $ "foldl (+) 0 [1, 2, 3, 4, 5]: " ++ show foldlExample1
  putStrLn $ "foldl (\\acc x -> acc ++ show x) \"\" [1, 2, 3, 4, 5]: " ++ show foldlExample2
  
  putStrLn $ "foldr (+) 0 [1, 2, 3, 4, 5]: " ++ show foldrExample1
  putStrLn $ "foldr (:) [] [1, 2, 3, 4, 5]: " ++ show foldrExample2
  
  putStrLn $ "zipWith (+) [1, 2, 3] [4, 5, 6]: " ++ show zipWithExample1
  putStrLn $ "zipWith (\\x y -> show x ++ \"-\" ++ y) [1, 2, 3] [\"a\", \"b\", \"c\"]: " ++ show zipWithExample2
  
  putStrLn $ "sortBy (\\x y -> compare y x) [3, 1, 4, 1, 5, 9, 2, 6]: " ++ show sortByExample1
  putStrLn $ "sortBy (\\(_, x) (_, y) -> compare x y) [(\"a\", 3), (\"b\", 1), (\"c\", 2)]: " ++ show sortByExample2

-- Пример 2: Функции, возвращающие функции
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Функции, возвращающие функции"
  
  putStrLn $ "add 3 4: " ++ show (add 3 4)
  putStrLn $ "add5 10: " ++ show (add5 10)
  
  putStrLn $ "makeAdder 7 3: " ++ show ((makeAdder 7) 3)
  putStrLn $ "add10 5: " ++ show (add10 5)
  
  putStrLn $ "makeMultiplier 2 3 4: " ++ show ((makeMultiplier 2) 3 4)
  putStrLn $ "double 3 4: " ++ show (double 3 4)

-- Пример 3: Композиция функций
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Композиция функций"
  
  putStrLn $ "map ((*2) . (+1)) [1, 2, 3, 4, 5]: " ++ show compositionExample1
  putStrLn $ "map (reverse . map toUpper) [\"hello\", \"world\"]: " ++ show compositionExample2
  
  putStrLn $ "applyAll [(+1), (*2), (^2)] 3: " ++ show applyAllExample

-- Пример 4: Функции высших порядков для работы с функциями
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Функции высших порядков для работы с функциями"
  
  putStrLn $ "map (flip (++) \"!\") [\"hello\", \"world\"]: " ++ show flipExample1
  putStrLn $ "map (flip (,) 1) [1, 2, 3]: " ++ show flipExample2
  
  putStrLn $ "curry (\\(x, y) -> x + y) 3 4: " ++ show curryExample
  putStrLn $ "uncurry (+) (3, 4): " ++ show uncurryExample

-- Пример 5: Реализация стандартных функций высших порядков
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Реализация стандартных функций высших порядков"
  
  putStrLn $ "myMap (* 2) [1, 2, 3, 4, 5]: " ++ show (myMap (* 2) [1, 2, 3, 4, 5])
  putStrLn $ "myFilter even [1, 2, 3, 4, 5, 6]: " ++ show (myFilter even [1, 2, 3, 4, 5, 6])
  putStrLn $ "myFoldl (+) 0 [1, 2, 3, 4, 5]: " ++ show (myFoldl (+) 0 [1, 2, 3, 4, 5])
  putStrLn $ "myFoldr (+) 0 [1, 2, 3, 4, 5]: " ++ show (myFoldr (+) 0 [1, 2, 3, 4, 5])
  putStrLn $ "myZipWith (+) [1, 2, 3] [4, 5, 6]: " ++ show (myZipWith (+) [1, 2, 3] [4, 5, 6])

-- Пример 6: Функции высших порядков для обработки данных
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Функции высших порядков для обработки данных"
  
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  putStrLn $ "groupBy even [1..10]: " ++ show (groupBy even numbers)
  putStrLn $ "countIf even [1..10]: " ++ show (countIf even numbers)
  putStrLn $ "findFirst (> 5) [1..10]: " ++ show (findFirst (> 5) numbers)
  putStrLn $ "mapIf even (* 10) [1..10]: " ++ show (mapIf even (* 10) numbers)

-- Пример 7: Функции высших порядков для комбинаторных задач
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Функции высших порядков для комбинаторных задач"
  
  putStrLn $ "allPairs [1, 2] ['a', 'b']: " ++ show (allPairs [1, 2] ['a', 'b'])
  putStrLn $ "combinations 2 [1, 2, 3, 4]: " ++ show (combinations 2 [1, 2, 3, 4])
  putStrLn $ "permutations [1, 2, 3]: " ++ show (permutations [1, 2, 3])

-- Главная функция
main :: IO ()
main = do
  putStrLn "Функции высших порядков в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  
  putStrLn "\nКлючевые моменты о функциях высших порядков:"
  putStrLn "1. Функции высших порядков принимают функции в качестве аргументов или возвращают функции"
  putStrLn "2. Они позволяют абстрагировать общие паттерны и создавать более модульный код"
  putStrLn "3. Стандартные функции высших порядков включают map, filter, fold, zipWith и другие"
  putStrLn "4. Частичное применение функций позволяет создавать новые функции из существующих"
  putStrLn "5. Композиция функций позволяет комбинировать функции для создания новых функций"
  putStrLn "6. Функции высших порядков делают код более декларативным и выразительным"
  putStrLn "7. Они являются основой для многих функциональных паттернов программирования"
  putStrLn "8. Понимание функций высших порядков важно для эффективного программирования на Haskell"
