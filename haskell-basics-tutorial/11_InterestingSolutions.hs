-- | Интересные решения известных задач на Haskell
--
-- В этом модуле представлены элегантные решения известных алгоритмических задач,
-- демонстрирующие выразительность и мощь языка Haskell.
--
-- Здесь вы найдете примеры, которые показывают, как функциональный подход
-- и особенности Haskell (ленивые вычисления, сопоставление с образцом,
-- функции высшего порядка) позволяют создавать краткие и понятные решения
-- сложных задач.

module Main where

import Data.List (sort, nub, (\\))
import Data.Char (digitToInt)
import Control.Monad (guard)
import Data.Ratio ((%))
import Data.Array (Array, (!), array)

-- * Числа Фибоначчи

-- | Наивная рекурсивная реализация чисел Фибоначчи
-- Эта реализация проста, но неэффективна из-за повторных вычислений
fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | Эффективная реализация чисел Фибоначчи с использованием мемоизации
-- Благодаря ленивым вычислениям, каждое число Фибоначчи вычисляется только один раз
fibFast :: Int -> Integer
fibFast n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Бесконечный список чисел Фибоначчи
-- Демонстрирует мощь ленивых вычислений в Haskell
fibonacciList :: [Integer]
fibonacciList = 0 : 1 : zipWith (+) fibonacciList (tail fibonacciList)

-- | Вычисление чисел Фибоначчи с использованием свертки
-- Элегантное решение с использованием функций высшего порядка
fibFold :: Int -> Integer
fibFold n = snd $ foldl (\(a, b) _ -> (b, a + b)) (0, 1) [1..n-1]

-- * Простые числа

-- | Проверка, является ли число простым
-- Простая реализация, проверяющая делители до квадратного корня
isPrime :: Int -> Bool
isPrime n | n <= 1    = False
          | n <= 3    = True
          | even n    = False
          | otherwise = all (\x -> n `mod` x /= 0) [3, 5..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

-- | Решето Эратосфена для генерации простых чисел
-- Классический алгоритм, элегантно реализованный с использованием ленивых списков
sieveOfEratosthenes :: [Int]
sieveOfEratosthenes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

-- | Получение n-го простого числа
-- Использует бесконечный список простых чисел
nthPrime :: Int -> Int
nthPrime n = sieveOfEratosthenes !! (n-1)

-- * Алгоритмы сортировки

-- | Быстрая сортировка (Quicksort)
-- Классический алгоритм, выраженный очень лаконично на Haskell
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]

-- | Сортировка слиянием (Merge Sort)
-- Еще один классический алгоритм сортировки с элегантной реализацией
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- * Задача о Ханойских башнях

-- | Решение задачи о Ханойских башнях
-- Классическая рекурсивная задача, элегантно решаемая на Haskell
hanoi :: Int -> String -> String -> String -> [(String, String)]
hanoi 0 _ _ _ = []
hanoi n from to aux = 
    hanoi (n-1) from aux to ++ [(from, to)] ++ hanoi (n-1) aux to from

-- | Вывод решения задачи о Ханойских башнях в читаемом формате
printHanoi :: Int -> IO ()
printHanoi n = mapM_ (\(from, to) -> putStrLn $ "Переместить диск с " ++ from ++ " на " ++ to) $ 
               hanoi n "A" "C" "B"

-- * Задача о N ферзях

-- | Решение задачи о расстановке N ферзей на шахматной доске
-- Демонстрирует использование монады списка для элегантного решения задачи перебора
queensProblem :: Int -> [[Int]]
queensProblem n = queens n
  where
    queens 0 = [[]]
    queens k = do
      qs <- queens (k-1)
      q <- [1..n] \\ qs
      guard $ safe q qs
      return (q:qs)
    
    safe q qs = all (\(i, qi) -> q /= qi && abs (q - qi) /= i) $ zip [1..] qs

-- | Вывод решения задачи о N ферзях в виде шахматной доски
printQueensSolution :: [Int] -> IO ()
printQueensSolution qs = 
    mapM_ (\row -> putStrLn [if col == qs !! (row-1) then 'Q' else '.' | col <- [1..length qs]]) [1..length qs]

-- * Разбор выражений

-- | Простой парсер арифметических выражений
-- Демонстрирует рекурсивный спуск и обработку строк в функциональном стиле
data Expr = Num Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          deriving (Show)

-- | Вычисление значения выражения
evalExpr :: Expr -> Int
evalExpr (Num n) = n
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 `div` evalExpr e2

-- | Простой парсер для арифметических выражений
-- Обратите внимание, что это очень упрощенная версия без обработки ошибок
parseExpr :: String -> Expr
parseExpr = fst . parseExpr' . filter (/= ' ')
  where
    parseExpr' :: String -> (Expr, String)
    parseExpr' s = parseTerm s
    
    parseTerm :: String -> (Expr, String)
    parseTerm s = 
      let (e, s') = parseFactor s
      in case s' of
           ('+':rest) -> let (e', s'') = parseTerm rest in (Add e e', s'')
           ('-':rest) -> let (e', s'') = parseTerm rest in (Sub e e', s'')
           _ -> (e, s')
    
    parseFactor :: String -> (Expr, String)
    parseFactor s = 
      let (e, s') = parseAtom s
      in case s' of
           ('*':rest) -> let (e', s'') = parseFactor rest in (Mul e e', s'')
           ('/':rest) -> let (e', s'') = parseFactor rest in (Div e e', s'')
           _ -> (e, s')
    
    parseAtom :: String -> (Expr, String)
    parseAtom ('(':s) = 
      let (e, s') = parseExpr' s
      in case s' of
           (')':rest) -> (e, rest)
           _ -> error "Ожидалась закрывающая скобка"
    parseAtom s = 
      let (digits, rest) = span (`elem` "0123456789") s
      in (Num (read digits), rest)

-- * Задача о рюкзаке (Knapsack Problem)

-- | Решение задачи о рюкзаке с использованием динамического программирования
-- Демонстрирует использование мемоизации в функциональном стиле
knapsack :: [(Int, Int)] -> Int -> Int
knapsack items capacity = table ! (length items, capacity)
  where
    table = array ((0, 0), (length items, capacity)) 
            [((i, c), compute i c) | i <- [0..length items], c <- [0..capacity]]
    
    compute 0 _ = 0
    compute i c
      | weight > c = table ! (i-1, c)
      | otherwise  = max (table ! (i-1, c)) (value + table ! (i-1, c-weight))
      where (weight, value) = items !! (i-1)

-- * Функциональные структуры данных

-- | Реализация сбалансированного бинарного дерева поиска (AVL-дерево)
-- Демонстрирует, как можно реализовать сложные структуры данных в функциональном стиле
data AVLTree a = Empty
               | Node a Int (AVLTree a) (AVLTree a)
               deriving (Show)

-- | Высота дерева
height :: AVLTree a -> Int
height Empty = 0
height (Node _ h _ _) = h

-- | Создание нового узла
node :: a -> AVLTree a -> AVLTree a -> AVLTree a
node x left right = Node x (1 + max (height left) (height right)) left right

-- | Вставка элемента в AVL-дерево с поддержанием баланса
insertAVL :: Ord a => a -> AVLTree a -> AVLTree a
insertAVL x Empty = node x Empty Empty
insertAVL x (Node y h left right)
  | x < y     = balance y (insertAVL x left) right
  | x > y     = balance y left (insertAVL x right)
  | otherwise = Node y h left right
  where
    -- Балансировка дерева
    balance :: a -> AVLTree a -> AVLTree a -> AVLTree a
    balance y left right
      | heightDiff > 1  = 
          case left of
            Node z _ leftLeft leftRight
              | height leftLeft >= height leftRight -> rotateRight y left right
              | otherwise -> doubleRotateLeftRight y left right
            _ -> error "Невозможный случай"
      | heightDiff < -1 = 
          case right of
            Node z _ rightLeft rightRight
              | height rightRight >= height rightLeft -> rotateLeft y left right
              | otherwise -> doubleRotateRightLeft y left right
            _ -> error "Невозможный случай"
      | otherwise = node y left right
      where
        heightDiff = height left - height right
    
    -- Правый поворот
    rotateRight :: a -> AVLTree a -> AVLTree a -> AVLTree a
    rotateRight y (Node x _ leftX rightX) right = node x leftX (node y rightX right)
    rotateRight _ _ _ = error "Невозможный случай"
    
    -- Левый поворот
    rotateLeft :: a -> AVLTree a -> AVLTree a -> AVLTree a
    rotateLeft y left (Node x _ leftX rightX) = node x (node y left leftX) rightX
    rotateLeft _ _ _ = error "Невозможный случай"
    
    -- Двойной поворот (лево-право)
    doubleRotateLeftRight :: a -> AVLTree a -> AVLTree a -> AVLTree a
    doubleRotateLeftRight y (Node x _ leftX (Node z _ leftZ rightZ)) right =
      node z (node x leftX leftZ) (node y rightZ right)
    doubleRotateLeftRight _ _ _ = error "Невозможный случай"
    
    -- Двойной поворот (право-лево)
    doubleRotateRightLeft :: a -> AVLTree a -> AVLTree a -> AVLTree a
    doubleRotateRightLeft y left (Node x _ (Node z _ leftZ rightZ) rightX) =
      node z (node y left leftZ) (node x rightZ rightX)
    doubleRotateRightLeft _ _ _ = error "Невозможный случай"

-- | Проверка, содержится ли элемент в AVL-дереве
containsAVL :: Ord a => a -> AVLTree a -> Bool
containsAVL _ Empty = False
containsAVL x (Node y _ left right)
  | x < y     = containsAVL x left
  | x > y     = containsAVL x right
  | otherwise = True

-- | Преобразование AVL-дерева в отсортированный список
toListAVL :: AVLTree a -> [a]
toListAVL Empty = []
toListAVL (Node x _ left right) = toListAVL left ++ [x] ++ toListAVL right

-- * Задача о разбиении множества (Subset Sum Problem)

-- | Решение задачи о разбиении множества
-- Проверяет, можно ли разбить множество на два подмножества с равной суммой
canPartition :: [Int] -> Bool
canPartition nums
  | odd total = False  -- Если сумма нечетная, разбиение невозможно
  | otherwise = subsetSum nums (total `div` 2)
  where
    total = sum nums
    
    -- Проверяет, существует ли подмножество с заданной суммой
    subsetSum :: [Int] -> Int -> Bool
    subsetSum nums target = dp ! target
      where
        dp = array (0, target) [(s, compute s) | s <- [0..target]]
        
        compute 0 = True
        compute s = any (\num -> s >= num && dp ! (s - num)) nums

-- * Задача о наибольшей общей подпоследовательности (LCS)

-- | Нахождение наибольшей общей подпоследовательности двух строк
-- Демонстрирует динамическое программирование в функциональном стиле
longestCommonSubsequence :: String -> String -> String
longestCommonSubsequence xs ys = reverse $ lcs (length xs) (length ys)
  where
    lcs i j
      | i == 0 || j == 0 = ""
      | xs !! (i-1) == ys !! (j-1) = xs !! (i-1) : lcs (i-1) (j-1)
      | otherwise = if length (lcs i (j-1)) > length (lcs (i-1) j)
                    then lcs i (j-1)
                    else lcs (i-1) j

-- * Задача о редакционном расстоянии (Edit Distance)

-- | Вычисление редакционного расстояния между двумя строками
-- Минимальное количество операций (вставка, удаление, замена), 
-- необходимых для преобразования одной строки в другую
editDistance :: String -> String -> Int
editDistance xs ys = table ! (length xs, length ys)
  where
    table = array ((0, 0), (length xs, length ys)) 
            [((i, j), compute i j) | i <- [0..length xs], j <- [0..length ys]]
    
    compute 0 j = j  -- Вставка j символов
    compute i 0 = i  -- Удаление i символов
    compute i j
      | xs !! (i-1) == ys !! (j-1) = table ! (i-1, j-1)
      | otherwise = 1 + minimum [table ! (i-1, j),    -- Удаление
                                 table ! (i, j-1),    -- Вставка
                                 table ! (i-1, j-1)]  -- Замена

-- * Задача о генерации всех перестановок

-- | Генерация всех перестановок списка
-- Демонстрирует элегантное решение с использованием рекурсии
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [x:p | x <- xs, p <- permutations (delete x xs)]
  where
    delete :: Eq a => a -> [a] -> [a]
    delete _ [] = []
    delete y (z:zs)
      | y == z    = zs
      | otherwise = z : delete y zs

-- * Задача о генерации всех подмножеств

-- | Генерация всех подмножеств списка (множества)
-- Демонстрирует мощь списковых включений
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

-- * Задача о генерации чисел Каталана

-- | Генерация чисел Каталана
-- Числа Каталана возникают во многих комбинаторных задачах
catalanNumbers :: [Integer]
catalanNumbers = 1 : [c n | n <- [1..]]
  where
    c n = sum [catalanNumbers !! i * catalanNumbers !! (n-1-i) | i <- [0..n-1]]

-- | Вычисление n-го числа Каталана по формуле
catalanNumber :: Int -> Integer
catalanNumber n = factorial (2*n) `div` (factorial (n+1) * factorial n)
  where
    factorial m = product [1..fromIntegral m]

-- * Задача о разбиении числа на слагаемые

-- | Количество способов разбить число n на сумму положительных целых чисел
-- Например, число 4 можно разбить на: 4, 3+1, 2+2, 2+1+1, 1+1+1+1
partitions :: Int -> Int
partitions n = table ! n
  where
    table = array (0, n) [(i, compute i) | i <- [0..n]]
    
    compute 0 = 1
    compute i = sum [table ! (i - j) | j <- [1..i]]

-- * Задача о поиске пути в графе

-- | Поиск пути в графе с использованием поиска в глубину (DFS)
-- Граф представлен в виде списка смежности
type Graph = [(Int, [Int])]

-- | Поиск пути из вершины start в вершину end в графе
findPath :: Graph -> Int -> Int -> Maybe [Int]
findPath graph start end = dfs [start] start
  where
    -- Получение соседей вершины
    neighbors v = case lookup v graph of
                    Just ns -> ns
                    Nothing -> []
    
    -- Поиск в глубину
    dfs visited v
      | v == end = Just [v]
      | otherwise = case filter (`notElem` visited) (neighbors v) of
                      [] -> Nothing
                      ns -> case firstJust [dfs (v:visited) n | n <- ns] of
                              Just path -> Just (v:path)
                              Nothing -> Nothing
    
    -- Возвращает первый Just из списка Maybe
    firstJust [] = Nothing
    firstJust (Just x:_) = Just x
    firstJust (Nothing:xs) = firstJust xs

-- * Задача о вычислении чисел Пелля

-- | Числа Пелля - последовательность целых чисел, определяемая рекуррентным соотношением
-- P(0) = 0, P(1) = 1, P(n) = 2 * P(n-1) + P(n-2)
pellNumbers :: [Integer]
pellNumbers = 0 : 1 : zipWith (\a b -> 2*b + a) pellNumbers (tail pellNumbers)

-- | Вычисление n-го числа Пелля
pellNumber :: Int -> Integer
pellNumber n = pellNumbers !! n

-- * Задача о вычислении чисел Падована

-- | Последовательность Падована - последовательность, определяемая рекуррентным соотношением
-- P(0) = P(1) = P(2) = 1, P(n) = P(n-2) + P(n-3)
padovanSequence :: [Integer]
padovanSequence = 1 : 1 : 1 : zipWith (+) padovanSequence (tail padovanSequence)

-- | Вычисление n-го числа Падована
padovanNumber :: Int -> Integer
padovanNumber n = padovanSequence !! n

-- * Задача о вычислении треугольных чисел

-- | Треугольные числа - последовательность чисел, представляющих количество точек,
-- которые могут быть расположены в форме равностороннего треугольника
triangularNumbers :: [Integer]
triangularNumbers = scanl (+) 1 [2..]

-- | Вычисление n-го треугольного числа
triangularNumber :: Int -> Integer
triangularNumber n = n' * (n' + 1) `div` 2
  where n' = fromIntegral n

-- * Задача о вычислении чисел Фибоначчи с использованием матриц

-- | Вычисление n-го числа Фибоначчи за O(log n) с использованием возведения матрицы в степень
fibMatrix :: Int -> Integer
fibMatrix n
  | n == 0    = 0
  | otherwise = let (_, b, _, _) = matrixPower (1, 1, 1, 0) (n-1) in b
  where
    -- Умножение матриц 2x2
    matrixMult (a, b, c, d) (e, f, g, h) = (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)
    
    -- Возведение матрицы в степень
    matrixPower m 0 = (1, 0, 0, 1)  -- Единичная матрица
    matrixPower m 1 = m
    matrixPower m n
      | even n    = matrixPower (matrixMult m m) (n `div` 2)
      | otherwise = matrixMult m (matrixPower (matrixMult m m) (n `div` 2))

-- * Задача о вычислении биномиальных коэффициентов

-- | Вычисление биномиального коэффициента (n choose k)
binomial :: Integer -> Integer -> Integer
binomial n k
  | k < 0 || k > n = 0
  | k == 0 || k == n = 1
  | k > n - k = binomial n (n - k)
  | otherwise = product [n-k+1..n] `div` product [1..k]

-- | Треугольник Паскаля - таблица биномиальных коэффициентов
pascalsTriangle :: [[Integer]]
pascalsTriangle = iterate nextRow [1]
  where
    nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

-- * Задача о вычислении факториала

-- | Вычисление факториала числа
factorial :: Integer -> Integer
factorial n = product [1..n]

-- | Вычисление факториала с использованием хвостовой рекурсии
factorialTail :: Integer -> Integer
factorialTail n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n-1) (n*acc)

-- * Задача о вычислении НОД и НОК

-- | Нахождение наибольшего общего делителя (алгоритм Евклида)
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = abs a
gcd' a b = gcd' b (a `mod` b)

-- | Нахождение наименьшего общего кратного
lcm' :: Integer -> Integer -> Integer
lcm' a b = abs (a * b) `div` gcd' a b

-- * Задача о проверке числа на совершенство

-- | Проверка, является ли число совершенным
-- Совершенное число равно сумме своих собственных делителей
isPerfectNumber :: Int -> Bool
isPerfectNumber n = n == sum [d | d <- [1..n-1], n `mod` d == 0]

-- | Генерация совершенных чисел (формула Евклида-Эйлера)
-- Если 2^p - 1 простое (число Мерсенна), то 2^(p-1) * (2^p - 1) - совершенное число
perfectNumbers :: [Integer]
perfectNumbers = [2^(p-1) * (2^p - 1) | p <- [2..], isPrime' (2^p - 1)]
  where
    isPrime' n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor . sqrt $ fromIntegral n]

-- * Задача о разложении числа на простые множители

-- | Разложение числа на простые множители
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    primes = 2 : filter (\x -> all (\p -> x `mod` p /= 0) [2..floor . sqrt $ fromIntegral x]) [3,5..]
    
    factor 1 _ = []
    factor n (p:ps)
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | p * p > n      = [n]
      | otherwise      = factor n ps

-- * Задача о вычислении суммы цифр числа

-- | Вычисление суммы цифр числа
sumOfDigits :: Integer -> Integer
sumOfDigits n = sum $ map (toInteger . digitToInt) $ show n

-- * Задача о проверке числа на палиндромность

-- | Проверка, является ли число палиндромом
isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

-- * Задача о вычислении числа Армстронга

-- | Проверка, является ли число числом Армстронга
-- Число Армстронга - число, равное сумме своих цифр, возведенных в степень количества цифр
isArmstrongNumber :: Integer -> Bool
isArmstrongNumber n = n == sum [toInteger (digitToInt d) ^ numDigits | d <- show n]
  where
    numDigits = length $ show n

-- * Задача о вычислении чисел Колатца

-- | Последовательность Колатца для числа n
-- Если n четное, то следующее число n/2
-- Если n нечетное, то следующее число 3*n + 1
collatzSequence :: Integer -> [Integer]
collatzSequence 1 = [1]
collatzSequence n
  | even n    = n : collatzSequence (n `div` 2)
  | otherwise = n : collatzSequence (3*n + 1)

-- | Длина последовательности Колатца для числа n
collatzLength :: Integer -> Int
collatzLength = length . collatzSequence

-- * Задача о вычислении чисел Люка

-- | Числа Люка - последовательность, похожая на числа Фибоначчи
-- L(0) = 2, L(1) = 1, L(n) = L(n-1) + L(n-2)
lucasNumbers :: [Integer]
lucasNumbers = 2 : 1 : zipWith (+) lucasNumbers (tail lucasNumbers)

-- | Вычисление n-го числа Люка
lucasNumber :: Int -> Integer
lucasNumber n = lucasNumbers !! n

-- * Задача о вычислении чисел Леонардо

-- | Числа Леонардо - последовательность, определяемая рекуррентным соотношением
-- L(0) = 1, L(1) = 1, L(n) = L(n-1) + L(n-2) + 1
leonardoNumbers :: [Integer]
leonardoNumbers = 1 : 1 : zipWith (\a b -> a + b + 1) leonardoNumbers (tail leonardoNumbers)

-- | Вычисление n-го числа Леонардо
leonardoNumber :: Int -> Integer
leonardoNumber n = leonardoNumbers !! n

-- * Задача о генерации всех правильных скобочных последовательностей

-- | Генерация всех правильных скобочных последовательностей длины 2*n
-- Использует числа Каталана и комбинаторный подход
balancedParentheses :: Int -> [String]
balancedParentheses n = generateParens n n ""
  where
    generateParens 0 0 acc = [acc]
    generateParens open close acc
      | open == close = generateParens (open-1) close (acc ++ "(")
      | open == 0 = generateParens open (close-1) (acc ++ ")")
      | open < close = generateParens (open-1) close (acc ++ "(") ++ 
                       generateParens open (close-1) (acc ++ ")")
      | otherwise = []

-- * Задача о вычислении расстояния Левенштейна

-- | Вычисление расстояния Левенштейна между двумя строками
-- Минимальное количество операций (вставка, удаление, замена),
-- необходимых для преобразования одной строки в другую
levenshteinDistance :: String -> String -> Int
levenshteinDistance = editDistance  -- Это то же самое, что и editDistance

-- * Задача о вычислении чисел Стирлинга

-- | Числа Стирлинга первого рода - количество перестановок из n элементов,
-- которые имеют k циклов
stirling1 :: Int -> Int -> Integer
stirling1 n k
  | n == 0 && k == 0 = 1
  | n == 0 || k == 0 = 0
  | otherwise = fromIntegral (n-1) * stirling1 (n-1) k + stirling1 (n-1) (k-1)

-- | Числа Стирлинга второго рода - количество способов разбить множество
-- из n элементов на k непустых подмножеств
stirling2 :: Int -> Int -> Integer
stirling2 n k
  | n == 0 && k == 0 = 1
  | n == 0 || k == 0 = 0
  | k == 1 || k == n = 1
  | otherwise = fromIntegral k * stirling2 (n-1) k + stirling2 (n-1) (k-1)

-- * Задача о вычислении чисел Белла

-- | Числа Белла - количество способов разбить множество из n элементов
-- на непустые подмножества
bellNumbers :: [Integer]
bellNumbers = 1 : [sum [stirling2 n k * factorial (fromIntegral k) | k <- [1..n]] | n <- [1..]]

-- | Вычисление n-го числа Белла
bellNumber :: Int -> Integer
bellNumber n = bellNumbers !! n

-- * Задача о вычислении чисел Бернулли

-- | Числа Бернулли - последовательность рациональных чисел с важными свойствами
-- в теории чисел и анализе
bernoulliNumbers :: [Rational]
bernoulliNumbers = 1 : 
                   (-1) % 2 : 
                   [sum [fromIntegral (binomial (fromIntegral n+1) (fromIntegral k)) * (bernoulliNumbers !! fromIntegral k) | k <- [0..n-1]] * 
                    ((-1) % fromIntegral (n+1)) | n <- [2..]]

-- * Задача о вычислении чисел Моцкина

-- | Числа Моцкина - количество способов соединить точки на окружности
-- непересекающимися хордами, при этом некоторые точки могут остаться несоединенными
motzkinNumbers :: [Integer]
motzkinNumbers = 1 : 1 : 
                 [sum [motzkinNumbers !! (n-1-k) * 
                       (motzkinNumbers !! k + motzkinNumbers !! (k-1)) | 
                       k <- [1..n-1]] `div` (fromIntegral n + 2) | 
                  n <- [2..]]

-- | Вычисление n-го числа Моцкина
motzkinNumber :: Int -> Integer
motzkinNumber n = motzkinNumbers !! n

-- * Задача о вычислении чисел Делануа

-- | Числа Делануа - количество путей из (0,0) в (m,n), которые никогда не поднимаются
-- выше диагонали y = x, используя только шаги вправо, вверх и по диагонали
delanoyNumbers :: [[Integer]]
delanoyNumbers = iterate nextRow [1]
  where
    nextRow row = scanl (\acc x -> acc + x + (head row)) 0 row

-- | Вычисление числа Делануа D(m,n)
delanoyNumber :: Int -> Int -> Integer
delanoyNumber m n
  | m < 0 || n < 0 = 0
  | m == 0 && n == 0 = 1
  | otherwise = delanoyNumber (m-1) n + delanoyNumber m (n-1) + delanoyNumber (m-1) (n-1)

-- * Задача о вычислении полиномов Чебышева

-- | Полиномы Чебышева первого рода
chebyshevT :: Int -> Double -> Double
chebyshevT 0 _ = 1
chebyshevT 1 x = x
chebyshevT n x = 2 * x * chebyshevT (n-1) x - chebyshevT (n-2) x

-- | Полиномы Чебышева второго рода
chebyshevU :: Int -> Double -> Double
chebyshevU 0 _ = 1
chebyshevU 1 x = 2 * x
chebyshevU n x = 2 * x * chebyshevU (n-1) x - chebyshevU (n-2) x

-- * Задача о вычислении полиномов Лежандра

-- | Полиномы Лежандра
legendreP :: Int -> Double -> Double
legendreP 0 _ = 1
legendreP 1 x = x
legendreP n x = ((2*fromIntegral n - 1) * x * legendreP (n-1) x - 
                 (fromIntegral n - 1) * legendreP (n-2) x) / fromIntegral n

-- * Задача о вычислении полиномов Эрмита

-- | Полиномы Эрмита (физическая версия)
hermiteH :: Int -> Double -> Double
hermiteH 0 _ = 1
hermiteH 1 x = 2 * x
hermiteH n x = 2 * x * hermiteH (n-1) x - 2 * fromIntegral (n-1) * hermiteH (n-2) x

-- * Задача о вычислении полиномов Лагерра

-- | Полиномы Лагерра
laguerreL :: Int -> Double -> Double
laguerreL 0 _ = 1
laguerreL 1 x = 1 - x
laguerreL n x = ((2*fromIntegral n - 1 - x) * laguerreL (n-1) x - 
                 (fromIntegral n - 1) * laguerreL (n-2) x) / fromIntegral n

-- * Задача о вычислении чисел Эйлера

-- | Числа Эйлера первого рода - количество перестановок из n элементов с k подъемами
eulerNumbers :: [[Integer]]
eulerNumbers = iterate nextRow [1]
  where
    nextRow row = scanl (\acc (i, x) -> (fromIntegral (length row) - fromIntegral i + 1) * acc + 
                                        fromIntegral i * x) 
                        0 
                        (zip [0..] row)

-- | Вычисление числа Эйлера E(n,k)
eulerNumber :: Int -> Int -> Integer
eulerNumber n k
  | k < 0 || k > n = 0
  | n == 0 && k == 0 = 1
  | otherwise = (fromIntegral (n-k)) * eulerNumber (n-1) (k-1) + 
                (fromIntegral (k+1)) * eulerNumber (n-1) k

-- * Задача о вычислении чисел Эйлера второго рода

-- | Числа Эйлера второго рода - количество перестановок из n элементов с k подъемами
euler2Number :: Int -> Int -> Integer
euler2Number n k
  | k < 0 || k >= n = 0
  | n == 0 && k == 0 = 1
  | otherwise = (fromIntegral (k+1)) * euler2Number (n-1) k + 
                (fromIntegral (n-k)) * euler2Number (n-1) (k-1)

-- * Задача о вычислении чисел Нараяны

-- | Числа Нараяны - количество способов расставить n пар скобок так,
-- чтобы глубина вложенности не превышала k
narayanaNumber :: Int -> Int -> Integer
narayanaNumber n k
  | n <= 0 || k <= 0 || k > n = 0
  | otherwise = (binomial (fromIntegral n) (fromIntegral k) * binomial (fromIntegral n) (fromIntegral (k-1))) `div` fromIntegral n

-- * Задача о вычислении чисел Шрёдера

-- | Числа Шрёдера - количество путей из (0,0) в (n,n), которые никогда не поднимаются
-- выше диагонали y = x, используя только шаги вправо, вверх и по диагонали
schroderNumbers :: [Integer]
schroderNumbers = 1 : 1 : 
                  [3 * schroderNumbers !! (n-1) + 
                   sum [schroderNumbers !! k * schroderNumbers !! (n-2-k) | k <- [0..n-2]] | 
                   n <- [2..]]

-- | Вычисление n-го числа Шрёдера
schroderNumber :: Int -> Integer
schroderNumber n = schroderNumbers !! n

-- * Задача о вычислении чисел Деланнуа

-- | Числа Деланнуа - количество путей из (0,0) в (m,n), используя только шаги
-- вправо, вверх и по диагонали
delannoyNumber :: Int -> Int -> Integer
delannoyNumber m n
  | m < 0 || n < 0 = 0
  | m == 0 = 1
  | n == 0 = 1
  | otherwise = delannoyNumber (m-1) n + delannoyNumber m (n-1) + delannoyNumber (m-1) (n-1)

-- * Задача о вычислении чисел Фибоначчи-Пелля

-- | Числа Фибоначчи-Пелля - последовательность, определяемая рекуррентным соотношением
-- F(0) = 0, F(1) = 1, F(n) = 2 * F(n-1) + F(n-2)
fibonacciPellNumbers :: [Integer]
fibonacciPellNumbers = 0 : 1 : zipWith (\a b -> 2*b + a) fibonacciPellNumbers (tail fibonacciPellNumbers)

-- | Вычисление n-го числа Фибоначчи-Пелля
fibonacciPellNumber :: Int -> Integer
fibonacciPellNumber n = fibonacciPellNumbers !! n

-- * Задача о вычислении чисел Якобсталя

-- | Числа Якобсталя - последовательность, определяемая рекуррентным соотношением
-- J(0) = 0, J(1) = 1, J(n) = J(n-1) + 2 * J(n-2)
jacobsthalNumbers :: [Integer]
jacobsthalNumbers = 0 : 1 : zipWith (\a b -> b + 2*a) jacobsthalNumbers (tail jacobsthalNumbers)

-- | Вычисление n-го числа Якобсталя
jacobsthalNumber :: Int -> Integer
jacobsthalNumber n = jacobsthalNumbers !! n

-- * Задача о вычислении чисел Пизо

-- | Проверка, является ли число числом Пизо
-- Число Пизо - алгебраическое целое число больше 1, все сопряженные которого
-- по модулю меньше 1
isPisoNumber :: Double -> Bool
isPisoNumber x = x > 1 && isPisoPolynomial x
  where
    -- Это очень упрощенная проверка, в реальности нужно проверять корни многочлена
    isPisoPolynomial x = x^2 - x - 1 == 0 || x^3 - x - 1 == 0

-- * Задача о вычислении чисел Перрина

-- | Числа Перрина - последовательность, определяемая рекуррентным соотношением
-- P(0) = 3, P(1) = 0, P(2) = 2, P(n) = P(n-2) + P(n-3)
perrinNumbers :: [Integer]
perrinNumbers = 3 : 0 : 2 : zipWith (+) perrinNumbers (tail perrinNumbers)

-- | Вычисление n-го числа Перрина
perrinNumber :: Int -> Integer
perrinNumber n = perrinNumbers !! n

-- * Задача о вычислении чисел Падована

-- | Числа Падована - последовательность, определяемая рекуррентным соотношением
-- P(0) = P(1) = P(2) = 1, P(n) = P(n-2) + P(n-3)
padovanNumbers :: [Integer]
padovanNumbers = 1 : 1 : 1 : zipWith (+) padovanNumbers (tail padovanNumbers)

-- | Вычисление n-го числа Падована
padovanNumber' :: Int -> Integer
padovanNumber' n = padovanNumbers !! n

-- * Задача о вычислении чисел Пелля-Люка

-- | Числа Пелля-Люка - последовательность, определяемая рекуррентным соотношением
-- P(0) = 2, P(1) = 2, P(n) = 2 * P(n-1) + P(n-2)
pellLucasNumbers :: [Integer]
pellLucasNumbers = 2 : 2 : zipWith (\a b -> 2*b + a) pellLucasNumbers (tail pellLucasNumbers)

-- | Вычисление n-го числа Пелля-Люка
pellLucasNumber :: Int -> Integer
pellLucasNumber n = pellLucasNumbers !! n

-- * Задача о вычислении чисел Фибоначчи-Люка

-- | Числа Фибоначчи-Люка - последовательность, определяемая рекуррентным соотношением
-- F(0) = 2, F(1) = 1, F(n) = F(n-1) + F(n-2)
fibonacciLucasNumbers :: [Integer]
fibonacciLucasNumbers = 2 : 1 : zipWith (+) fibonacciLucasNumbers (tail fibonacciLucasNumbers)

-- | Вычисление n-го числа Фибоначчи-Люка
fibonacciLucasNumber :: Int -> Integer
fibonacciLucasNumber n = fibonacciLucasNumbers !! n

-- * Главная функция для демонстрации

-- | Главная функция, демонстрирующая некоторые из реализованных алгоритмов
main :: IO ()
main = do
  putStrLn "Демонстрация интересных решений на Haskell\n"
  
  putStrLn "Числа Фибоначчи (первые 10):"
  print $ take 10 fibonacciList
  
  putStrLn "\nПростые числа (первые 10):"
  print $ take 10 sieveOfEratosthenes
  
  putStrLn "\nБыстрая сортировка:"
  print $ quicksort [5, 1, 9, 4, 6, 7, 3]
  
  putStrLn "\nРешение задачи о Ханойских башнях для 3 дисков:"
  printHanoi 3
  
  putStrLn "\nРешение задачи о 4 ферзях:"
  let solution = head $ queensProblem 4
  printQueensSolution solution
  
  putStrLn "\nРазбор и вычисление выражения:"
  let expr = parseExpr "2*(3+4)-5"
  putStrLn $ "Выражение: 2*(3+4)-5"
  putStrLn $ "Результат: " ++ show (evalExpr expr)
  
  putStrLn "\nГенерация всех подмножеств [1,2,3]:"
  print $ subsets [1,2,3 :: Int]
  
  putStrLn "\nГенерация всех перестановок [1,2,3]:"
  print $ permutations [1,2,3 :: Int]
  
  putStrLn "\nЧисла Каталана (первые 10):"
  print $ take 10 catalanNumbers
  
  putStrLn "\nПоследовательность Колатца для числа 27:"
  print $ collatzSequence 27
