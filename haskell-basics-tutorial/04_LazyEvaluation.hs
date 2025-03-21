{-
  Ленивые вычисления в Haskell
  
  В этом файле мы рассмотрим ленивые вычисления (lazy evaluation),
  которые являются одной из ключевых особенностей Haskell.
-}

module Main where

import Data.List (foldl')
import Debug.Trace (trace)

-- Ленивые вычисления означают, что выражения вычисляются только тогда,
-- когда их значения действительно нужны. Это позволяет:
-- 1. Работать с бесконечными структурами данных
-- 2. Избегать ненужных вычислений
-- 3. Определять собственные управляющие структуры
-- 4. Улучшать производительность в некоторых случаях

-- Пример 1: Бесконечные списки

-- Бесконечный список всех натуральных чисел
naturals :: [Integer]
naturals = [1..]

-- Бесконечный список всех четных чисел
evens :: [Integer]
evens = [2, 4..]

-- Бесконечный список всех нечетных чисел
odds :: [Integer]
odds = [1, 3..]

-- Бесконечный список всех квадратов
squares :: [Integer]
squares = [x^2 | x <- [1..]]

-- Бесконечный список всех чисел Фибоначчи
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Бесконечный список всех простых чисел (решето Эратосфена)
primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- Пример 2: Ленивая оценка аргументов функций

-- Функция, которая возвращает первый аргумент и игнорирует второй
const' :: a -> b -> a
const' x _ = x

-- Функция, которая выбирает один из аргументов в зависимости от условия
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- Функция, которая вычисляет логическое И с коротким замыканием
(&&.) :: Bool -> Bool -> Bool
True  &&. b = b
False &&. _ = False

-- Функция, которая вычисляет логическое ИЛИ с коротким замыканием
(||.) :: Bool -> Bool -> Bool
True  ||. _ = True
False ||. b = b

-- Пример 3: Отложенные вычисления и thunks

-- Функция, которая выводит сообщение при вычислении и возвращает значение
traced :: Show a => String -> a -> a
traced msg x = trace (msg ++ ": " ++ show x) x

-- Функция, которая создает список с трассировкой
tracedList :: [Int]
tracedList = traced "Элемент 1" 1 : traced "Элемент 2" 2 : traced "Элемент 3" 3 : []

-- Функция, которая вычисляет сумму списка с трассировкой
sumTraced :: [Int] -> Int
sumTraced [] = 0
sumTraced (x:xs) = x + sumTraced xs

-- Пример 4: Строгие вычисления (strict evaluation)

-- Функция, которая вычисляет сумму списка строго (с помощью foldl')
strictSum :: [Int] -> Int
strictSum = foldl' (+) 0

-- Функция, которая принудительно вычисляет значение (с помощью seq)
forceEval :: a -> b -> b
forceEval a b = a `seq` b

-- Функция, которая вычисляет факториал строго
strictFactorial :: Integer -> Integer
strictFactorial n = go 1 n
  where
    go acc 0 = acc
    go acc m = acc `seq` go (acc * m) (m - 1)

-- Пример 5: Ленивые структуры данных

-- Бесконечное дерево
data Tree a = Node a (Tree a) (Tree a) deriving (Show)

-- Функция, которая создает бесконечное дерево
infiniteTree :: a -> Tree a
infiniteTree x = Node x (infiniteTree x) (infiniteTree x)

-- Функция, которая создает дерево с возрастающими значениями
countingTree :: Integer -> Tree Integer
countingTree n = Node n (countingTree (2 * n)) (countingTree (2 * n + 1))

-- Функция, которая обходит дерево в глубину до определенной глубины
-- Используем Maybe для обозначения, что глубина превышена
data LimitedTree a = LNode a (LimitedTree a) (LimitedTree a) | LLeaf a deriving (Show)

-- Преобразование обычного дерева в ограниченное дерево
takeTree :: Int -> Tree a -> LimitedTree a
takeTree 0 (Node x _ _) = LLeaf x
takeTree n (Node x left right) = LNode x (takeTree (n-1) left) (takeTree (n-1) right)

-- Пример 6: Мемоизация с помощью ленивых вычислений

-- Функция, которая вычисляет числа Фибоначчи наивно (экспоненциальная сложность)
fibNaive :: Integer -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

-- Функция, которая вычисляет числа Фибоначчи с мемоизацией (линейная сложность)
fibMemo :: Integer -> Integer
fibMemo n = fibs !! fromIntegral n
  where
    fibs :: [Integer]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Пример 7: Ленивые ввод-вывод

-- Функция, которая читает содержимое файла лениво
readFileLazy :: FilePath -> IO String
readFileLazy = readFile

-- Функция, которая обрабатывает содержимое файла построчно
processLines :: String -> [String]
processLines = lines

-- Функция, которая фильтрует строки по предикату
filterLines :: (String -> Bool) -> [String] -> [String]
filterLines p = filter p

-- Функция, которая объединяет строки с разделителем
joinLines :: [String] -> String
joinLines = unlines

-- Пример 1: Бесконечные списки
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Бесконечные списки"
  
  putStrLn $ "Первые 10 натуральных чисел: " ++ show (take 10 naturals)
  putStrLn $ "Первые 10 четных чисел: " ++ show (take 10 evens)
  putStrLn $ "Первые 10 нечетных чисел: " ++ show (take 10 odds)
  putStrLn $ "Первые 10 квадратов: " ++ show (take 10 squares)
  putStrLn $ "Первые 10 чисел Фибоначчи: " ++ show (take 10 fibs)
  putStrLn $ "Первые 10 простых чисел: " ++ show (take 10 primes)
  
  putStrLn $ "Сумма первых 1000 натуральных чисел: " ++ show (sum (take 1000 naturals))
  putStrLn $ "Произведение первых 10 натуральных чисел: " ++ show (product (take 10 naturals))
  
  putStrLn $ "Все четные числа от 1 до 20: " ++ show (filter even [1..20])
  putStrLn $ "Все числа от 1 до 100, кратные 7: " ++ show (filter (\x -> x `mod` 7 == 0) [1..100])

-- Пример 2: Ленивая оценка аргументов функций
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Ленивая оценка аргументов функций"
  
  putStrLn $ "const' 5 (error \"Этот аргумент не будет вычислен\"): " ++ show (const' 5 (length [1..]))
  
  putStrLn $ "if' True 5 (error \"Этот аргумент не будет вычислен\"): " ++ show (if' True 5 (length [1..]))
  putStrLn $ "if' False (error \"Этот аргумент не будет вычислен\") 10: " ++ show (if' False (length [1..]) 10)
  
  putStrLn $ "True &&. False: " ++ show (True &&. False)
  putStrLn $ "False &&. (error \"Этот аргумент не будет вычислен\"): " ++ show (False &&. (length [1..] > 0))
  
  putStrLn $ "True ||. (error \"Этот аргумент не будет вычислен\"): " ++ show (True ||. (length [1..] > 0))
  putStrLn $ "False ||. True: " ++ show (False ||. True)

-- Пример 3: Отложенные вычисления и thunks
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Отложенные вычисления и thunks"
  
  putStrLn "Создание списка tracedList:"
  let list = tracedList
  putStrLn "Список создан, но элементы еще не вычислены"
  
  putStrLn "\nВычисление первого элемента списка:"
  print (head list)
  
  putStrLn "\nВычисление суммы списка:"
  print (sumTraced list)
  
  putStrLn "\nСоздание большого списка и вычисление только первых элементов:"
  let bigList = [1..1000000]
  putStrLn $ "Первые 10 элементов: " ++ show (take 10 bigList)
  putStrLn "Остальные элементы не вычисляются"

-- Пример 4: Строгие вычисления
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Строгие вычисления"
  
  let list = [1, 2, 3, 4, 5]
  
  putStrLn $ "Ленивая сумма списка: " ++ show (sum list)
  putStrLn $ "Строгая сумма списка: " ++ show (strictSum list)
  
  putStrLn $ "Принудительное вычисление: " ++ show (forceEval (sum [1..1000]) "Результат")
  
  putStrLn $ "Ленивый факториал 10: " ++ show (product [1..10])
  putStrLn $ "Строгий факториал 10: " ++ show (strictFactorial 10)

-- Пример 5: Ленивые структуры данных
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Ленивые структуры данных"
  
  putStrLn "Бесконечное дерево (показываем только до глубины 2):"
  let tree = infiniteTree 1
  print (takeTree 2 tree)
  
  putStrLn "\nДерево с возрастающими значениями (показываем только до глубины 3):"
  let countTree = countingTree 1
  print (takeTree 3 countTree)

-- Пример 6: Мемоизация с помощью ленивых вычислений
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Мемоизация с помощью ленивых вычислений"
  
  putStrLn "Вычисление чисел Фибоначчи:"
  putStrLn $ "fibNaive 10: " ++ show (fibNaive 10)
  putStrLn $ "fibMemo 10: " ++ show (fibMemo 10)
  
  putStrLn $ "fibMemo 30: " ++ show (fibMemo 30)
  -- putStrLn $ "fibNaive 30: " ++ show (fibNaive 30)  -- Это займет очень много времени
  
  putStrLn "\nМемоизация позволяет избежать повторных вычислений"
  putStrLn "и значительно ускоряет рекурсивные функции"

-- Пример 7: Ленивые ввод-вывод
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Ленивые ввод-вывод"
  
  putStrLn "Ленивый ввод-вывод позволяет обрабатывать файлы построчно,"
  putStrLn "не загружая весь файл в память"
  
  putStrLn "\nПример обработки файла (не выполняется в этом примере):"
  putStrLn "readFileLazy \"большой_файл.txt\" >>= \\content ->"
  putStrLn "  let lines = processLines content"
  putStrLn "      filteredLines = filterLines (\\line -> length line > 10) lines"
  putStrLn "  in writeFile \"результат.txt\" (joinLines filteredLines)"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Ленивые вычисления в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  
  putStrLn "\nКлючевые моменты о ленивых вычислениях:"
  putStrLn "1. Ленивые вычисления означают, что выражения вычисляются только тогда, когда их значения действительно нужны"
  putStrLn "2. Это позволяет работать с бесконечными структурами данных"
  putStrLn "3. Ленивые вычисления позволяют избегать ненужных вычислений"
  putStrLn "4. Можно определять собственные управляющие структуры"
  putStrLn "5. Ленивые вычисления могут улучшать производительность в некоторых случаях"
  putStrLn "6. Однако ленивые вычисления могут приводить к утечкам памяти, если не использовать их осторожно"
  putStrLn "7. Строгие вычисления (strict evaluation) могут быть полезны в некоторых случаях"
  putStrLn "8. Мемоизация с помощью ленивых вычислений позволяет избежать повторных вычислений"
  putStrLn "9. Ленивый ввод-вывод позволяет обрабатывать большие файлы эффективно"
  putStrLn "10. Понимание ленивых вычислений важно для эффективного программирования на Haskell"
