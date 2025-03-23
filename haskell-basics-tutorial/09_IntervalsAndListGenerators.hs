{-
  Интервалы и генераторы списков в Haskell
  
  В этом файле мы рассмотрим различные способы создания и работы
  с интервалами и генераторами списков в Haskell.
-}

module Main where

import Data.Char (ord, chr, toUpper)
import Data.List (sort, sortBy, nub, group, groupBy, unfoldr)

-- Часть 1: Интервалы (Ranges)

-- Интервал - это последовательность значений, которая может быть выражена
-- с помощью синтаксиса [start..end] или [start,next..end]

-- Пример 1: Простые числовые интервалы
range1 :: [Int]
range1 = [1..10]  -- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

range2 :: [Int]
range2 = [10..1]  -- [] (пустой список, т.к. по умолчанию шаг равен 1)

range3 :: [Int]
range3 = [10,9..1]  -- [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]

-- Пример 2: Интервалы с шагом
range4 :: [Int]
range4 = [1,3..10]  -- [1, 3, 5, 7, 9]

range5 :: [Int]
range5 = [10,8..1]  -- [10, 8, 6, 4, 2]

range6 :: [Int]
range6 = [1,4..20]  -- [1, 4, 7, 10, 13, 16, 19]

-- Пример 3: Интервалы с числами с плавающей точкой
range7 :: [Double]
range7 = [0.1, 0.3 .. 1.0]  -- [0.1, 0.3, 0.5, 0.7, 0.9]

range8 :: [Double]
range8 = [1.0, 0.9 .. 0.0]  -- [1.0, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0.0]

-- Примечание: с числами с плавающей точкой могут возникать проблемы с точностью
range9 :: [Double]
range9 = [0.0, 0.1 .. 1.0]  -- может не включать 1.0 из-за ошибок округления

-- Пример 4: Символьные интервалы
range10 :: [Char]
range10 = ['a'..'z']  -- "abcdefghijklmnopqrstuvwxyz"

range11 :: [Char]
range11 = ['A'..'Z']  -- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

range12 :: [Char]
range12 = ['a','c'..'z']  -- "acegikmoqsuwy"

-- Пример 5: Бесконечные интервалы
-- Haskell использует ленивые вычисления, поэтому можно создавать бесконечные списки
range13 :: [Int]
range13 = [1..]  -- [1, 2, 3, ...] (бесконечный список)

range14 :: [Int]
range14 = [1,3..]  -- [1, 3, 5, ...] (бесконечный список с шагом 2)

-- Для работы с бесконечными списками используются функции, которые берут только часть списка
first10 :: [Int]
first10 = take 10 [1..]  -- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

first10Even :: [Int]
first10Even = take 10 [2,4..]  -- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

-- Часть 2: Списковые включения (List Comprehensions)

-- Списковое включение - это выражение, которое создает список на основе
-- существующих списков и условий

-- Пример 1: Простые списковые включения
squares :: [Int]
squares = [x^2 | x <- [1..10]]  -- [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

doubles :: [Int]
doubles = [2*x | x <- [1..10]]  -- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]

-- Пример 2: Списковые включения с условиями (фильтрами)
evenSquares :: [Int]
evenSquares = [x^2 | x <- [1..10], even x]  -- [4, 16, 36, 64, 100]

divisibleBy3And5 :: [Int]
divisibleBy3And5 = [x | x <- [1..100], x `mod` 3 == 0, x `mod` 5 == 0]  -- [15, 30, 45, 60, 75, 90]

-- Пример 3: Списковые включения с несколькими генераторами
coordinates :: [(Int, Int)]
coordinates = [(x, y) | x <- [1..3], y <- [1..3]]
-- [(1,1), (1,2), (1,3), (2,1), (2,2), (2,3), (3,1), (3,2), (3,3)]

-- Порядок генераторов важен
coordinates2 :: [(Int, Int)]
coordinates2 = [(x, y) | y <- [1..3], x <- [1..3]]
-- [(1,1), (2,1), (3,1), (1,2), (2,2), (3,2), (1,3), (2,3), (3,3)]

-- Пример 4: Зависимые генераторы
triangles :: [(Int, Int, Int)]
triangles = [(a, b, c) | a <- [1..10], b <- [a..10], c <- [b..10], a^2 + b^2 == c^2]
-- [(3,4,5), (6,8,10)]

-- Пример 5: Списковые включения с let-выражениями
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [(a, b, c) | 
                      m <- [2..10], 
                      n <- [1..m-1], 
                      let a = m^2 - n^2, 
                      let b = 2*m*n, 
                      let c = m^2 + n^2, 
                      a > 0, b > 0, c > 0]
-- [(3,4,5), (8,6,10), (5,12,13), (15,8,17), (12,16,20), (7,24,25), ...]

-- Пример 6: Списковые включения для строк
capitalizeWords :: String -> String
capitalizeWords str = unwords [toUpper c : cs | (c:cs) <- words str]

-- Пример 7: Вложенные списковые включения
matrix :: [[Int]]
matrix = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

flattenMatrix :: [Int]
flattenMatrix = [x | row <- matrix, x <- row]  -- [1, 2, 3, 4, 5, 6, 7, 8, 9]

-- Часть 3: Другие способы генерации списков

-- Пример 1: Функция replicate
replicated :: [Int]
replicated = replicate 5 42  -- [42, 42, 42, 42, 42]

-- Пример 2: Функция cycle
cycled :: [Int]
cycled = take 10 (cycle [1, 2, 3])  -- [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]

-- Пример 3: Функция repeat
repeated :: [Int]
repeated = take 5 (repeat 42)  -- [42, 42, 42, 42, 42]

-- Пример 4: Функция iterate
iterated :: [Int]
iterated = take 10 (iterate (*2) 1)  -- [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]

-- Пример 5: Функция unfoldr
fibonacciUnfoldr :: Int -> [Int]
fibonacciUnfoldr n = take n $ unfoldr (\(a, b) -> Just (a, (b, a + b))) (0, 1)
-- fibonacciUnfoldr 10 = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

-- Часть 4: Практические примеры использования генераторов списков

-- Пример 1: Решето Эратосфена для нахождения простых чисел
sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes :: [Int]
primes = sieve [2..]

primesUpTo :: Int -> [Int]
primesUpTo n = takeWhile (<= n) primes

-- Пример 2: Генерация всех подмножеств списка
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- Пример 3: Генерация всех перестановок списка
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:zs | y <- xs, zs <- permutations (filter (/= y) xs)]

-- Пример 4: Генерация всех пар элементов списка
pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | (i, x) <- zip [0..] xs, (j, y) <- zip [0..] xs, i < j]

-- Пример 5: Генерация всех возможных комбинаций элементов списка по n
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- Пример 6: Генерация таблицы умножения
multiplicationTable :: Int -> [[Int]]
multiplicationTable n = [[i * j | j <- [1..n]] | i <- [1..n]]

-- Пример 7: Генерация треугольника Паскаля
nextRow :: [Int] -> [Int]
nextRow row = zipWith (+) (0:row) (row ++ [0])

pascalTriangle :: Int -> [[Int]]
pascalTriangle n = take n $ iterate nextRow [1]

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Простые числовые интервалы"
  
  putStrLn $ "[1..10]: " ++ show range1
  putStrLn $ "[10..1]: " ++ show range2
  putStrLn $ "[10,9..1]: " ++ show range3

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Интервалы с шагом"
  
  putStrLn $ "[1,3..10]: " ++ show range4
  putStrLn $ "[10,8..1]: " ++ show range5
  putStrLn $ "[1,4..20]: " ++ show range6

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Интервалы с числами с плавающей точкой"
  
  putStrLn $ "[0.1, 0.3 .. 1.0]: " ++ show range7
  putStrLn $ "[1.0, 0.9 .. 0.0]: " ++ show range8
  putStrLn $ "[0.0, 0.1 .. 1.0]: " ++ show range9

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Символьные интервалы"
  
  putStrLn $ "['a'..'z']: " ++ show range10
  putStrLn $ "['A'..'Z']: " ++ show range11
  putStrLn $ "['a','c'..'z']: " ++ show range12

example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Бесконечные интервалы"
  
  putStrLn $ "take 10 [1..]: " ++ show first10
  putStrLn $ "take 10 [2,4..]: " ++ show first10Even

example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Простые списковые включения"
  
  putStrLn $ "[x^2 | x <- [1..10]]: " ++ show squares
  putStrLn $ "[2*x | x <- [1..10]]: " ++ show doubles

example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Списковые включения с условиями"
  
  putStrLn $ "[x^2 | x <- [1..10], even x]: " ++ show evenSquares
  putStrLn $ "[x | x <- [1..100], x `mod` 3 == 0, x `mod` 5 == 0]: " ++ show divisibleBy3And5

example8 :: IO ()
example8 = do
  putStrLn "\nПример 8: Списковые включения с несколькими генераторами"
  
  putStrLn $ "[(x, y) | x <- [1..3], y <- [1..3]]: " ++ show coordinates
  putStrLn $ "[(x, y) | y <- [1..3], x <- [1..3]]: " ++ show coordinates2

example9 :: IO ()
example9 = do
  putStrLn "\nПример 9: Зависимые генераторы"
  
  putStrLn $ "[(a, b, c) | a <- [1..10], b <- [a..10], c <- [b..10], a^2 + b^2 == c^2]: " ++ show triangles
  putStrLn $ "Первые 5 пифагоровых троек: " ++ show (take 5 pythagoreanTriples)

example10 :: IO ()
example10 = do
  putStrLn "\nПример 10: Списковые включения для строк"
  
  let sentence = "hello world haskell is awesome"
  putStrLn $ "Исходная строка: " ++ sentence
  putStrLn $ "Капитализация слов: " ++ capitalizeWords sentence

example11 :: IO ()
example11 = do
  putStrLn "\nПример 11: Вложенные списковые включения"
  
  putStrLn $ "Матрица: " ++ show matrix
  putStrLn $ "Сглаженная матрица: " ++ show flattenMatrix

example12 :: IO ()
example12 = do
  putStrLn "\nПример 12: Другие способы генерации списков"
  
  putStrLn $ "replicate 5 42: " ++ show replicated
  putStrLn $ "take 10 (cycle [1, 2, 3]): " ++ show cycled
  putStrLn $ "take 5 (repeat 42): " ++ show repeated
  putStrLn $ "take 10 (iterate (*2) 1): " ++ show iterated
  putStrLn $ "fibonacciUnfoldr 10: " ++ show (fibonacciUnfoldr 10)

example13 :: IO ()
example13 = do
  putStrLn "\nПример 13: Решето Эратосфена"
  
  putStrLn $ "Простые числа до 50: " ++ show (primesUpTo 50)

example14 :: IO ()
example14 = do
  putStrLn "\nПример 14: Генерация всех подмножеств списка"
  
  putStrLn $ "Все подмножества [1, 2, 3]: " ++ show (subsets [1, 2, 3 :: Int])

example15 :: IO ()
example15 = do
  putStrLn "\nПример 15: Генерация всех перестановок списка"
  
  putStrLn $ "Все перестановки [1, 2, 3]: " ++ show (permutations [1, 2, 3 :: Int])

example16 :: IO ()
example16 = do
  putStrLn "\nПример 16: Генерация всех пар элементов списка"
  
  putStrLn $ "Все пары из [1, 2, 3, 4]: " ++ show (pairs [1, 2, 3, 4 :: Int])

example17 :: IO ()
example17 = do
  putStrLn "\nПример 17: Генерация всех возможных комбинаций элементов списка по n"
  
  putStrLn $ "Все комбинации по 2 из [1, 2, 3, 4, 5]: " ++ show (combinations 2 [1, 2, 3, 4, 5 :: Int])

example18 :: IO ()
example18 = do
  putStrLn "\nПример 18: Генерация таблицы умножения"
  
  putStrLn "Таблица умножения 5x5:"
  mapM_ print (multiplicationTable 5)

example19 :: IO ()
example19 = do
  putStrLn "\nПример 19: Генерация треугольника Паскаля"
  
  putStrLn "Треугольник Паскаля (первые 6 строк):"
  mapM_ print (pascalTriangle 6)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Интервалы и генераторы списков в Haskell\n"
  
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
  example11
  example12
  example13
  example14
  example15
  example16
  example17
  example18
  example19
  
  putStrLn "\nКлючевые моменты об интервалах и генераторах списков:"
  putStrLn "1. Интервалы (ranges) позволяют создавать последовательности значений с помощью синтаксиса [start..end] или [start,next..end]"
  putStrLn "2. Интервалы могут быть числовыми, символьными и даже бесконечными"
  putStrLn "3. Списковые включения (list comprehensions) - мощный способ создания списков на основе существующих списков и условий"
  putStrLn "4. Синтаксис списковых включений: [выражение | генератор1, генератор2, ..., условие1, условие2, ...]"
  putStrLn "5. Генераторы в списковых включениях имеют вид переменная <- список"
  putStrLn "6. Порядок генераторов в списковых включениях важен и влияет на результат"
  putStrLn "7. В списковых включениях можно использовать let-выражения для создания локальных переменных"
  putStrLn "8. Функции replicate, cycle, repeat и iterate - альтернативные способы генерации списков"
  putStrLn "9. Функция unfoldr позволяет создавать списки на основе функции, которая генерирует следующий элемент"
  putStrLn "10. Генераторы списков широко используются в функциональном программировании для решения различных задач"
