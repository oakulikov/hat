{-
  Свёртка (Folding) в Haskell
  
  В этом файле мы рассмотрим свёртку (folding) - мощную концепцию
  функционального программирования для обработки структур данных.
-}

module Main where

import Data.Foldable (foldl', foldr', foldMap)
import Data.Monoid
import Data.List (sort, sortBy, nub, group, groupBy)

-- Часть 1: Основы свёртки

-- Свёртка (folding) - это процесс преобразования структуры данных
-- (обычно списка) в единственное значение путем последовательного
-- применения бинарной операции к элементам структуры и аккумулятору.

-- Пример 1: foldr - свёртка справа
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [x1, x2, ..., xn] = x1 `f` (x2 `f` ... (xn `f` z)...)

-- Сумма элементов списка с помощью foldr
sumFoldr :: Num a => [a] -> a
sumFoldr = foldr (+) 0

-- Произведение элементов списка с помощью foldr
productFoldr :: Num a => [a] -> a
productFoldr = foldr (*) 1

-- Конкатенация списков с помощью foldr
concatFoldr :: [[a]] -> [a]
concatFoldr = foldr (++) []

-- Пример 2: foldl - свёртка слева
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f z [x1, x2, ..., xn] = (...((z `f` x1) `f` x2) `f`...) `f` xn

-- Сумма элементов списка с помощью foldl
sumFoldl :: Num a => [a] -> a
sumFoldl = foldl (+) 0

-- Произведение элементов списка с помощью foldl
productFoldl :: Num a => [a] -> a
productFoldl = foldl (*) 1

-- Обращение списка с помощью foldl
reverseFoldl :: [a] -> [a]
reverseFoldl = foldl (\acc x -> x : acc) []

-- Пример 3: Строгие версии foldl и foldr
-- foldl' и foldr' - строгие версии foldl и foldr, которые
-- вычисляют аккумулятор на каждом шаге, избегая накопления отложенных вычислений

-- Сумма элементов списка с помощью foldl'
sumFoldl' :: Num a => [a] -> a
sumFoldl' = foldl' (+) 0

-- Произведение элементов списка с помощью foldl'
productFoldl' :: Num a => [a] -> a
productFoldl' = foldl' (*) 1

-- Часть 2: Реализация стандартных функций с помощью свёртки

-- Функция map, реализованная через foldr
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

-- Функция filter, реализованная через foldr
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x acc -> if p x then x : acc else acc) []

-- Функция any, реализованная через foldr
anyFoldr :: (a -> Bool) -> [a] -> Bool
anyFoldr p = foldr (\x acc -> p x || acc) False

-- Функция all, реализованная через foldr
allFoldr :: (a -> Bool) -> [a] -> Bool
allFoldr p = foldr (\x acc -> p x && acc) True

-- Функция length, реализованная через foldl
lengthFoldl :: [a] -> Int
lengthFoldl = foldl (\acc _ -> acc + 1) 0

-- Функция elem, реализованная через foldr
elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr x = foldr (\y acc -> y == x || acc) False

-- Часть 3: Другие виды свёртки

-- Пример 1: foldl1 и foldr1 - свёртки, которые используют первый элемент списка
-- в качестве начального значения аккумулятора
-- foldl1 :: (a -> a -> a) -> [a] -> a
-- foldr1 :: (a -> a -> a) -> [a] -> a

-- Максимальный элемент списка с помощью foldl1
maximumFoldl1 :: Ord a => [a] -> a
maximumFoldl1 = foldl1 max

-- Минимальный элемент списка с помощью foldl1
minimumFoldl1 :: Ord a => [a] -> a
minimumFoldl1 = foldl1 min

-- Пример 2: scanl и scanr - функции, которые возвращают все промежуточные
-- значения аккумулятора в виде списка
-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- scanr :: (a -> b -> b) -> b -> [a] -> [b]

-- Частичные суммы списка с помощью scanl
partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

-- Частичные произведения списка с помощью scanl
partialProducts :: Num a => [a] -> [a]
partialProducts = scanl (*) 1

-- Пример 3: foldMap - свёртка с использованием моноида
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m

-- Сумма элементов списка с помощью foldMap
sumFoldMap :: Num a => [a] -> a
sumFoldMap = getSum . foldMap Sum

-- Произведение элементов списка с помощью foldMap
productFoldMap :: Num a => [a] -> a
productFoldMap = getProduct . foldMap Product

-- Проверка, содержит ли список хотя бы один элемент, удовлетворяющий предикату
anyFoldMap :: (a -> Bool) -> [a] -> Bool
anyFoldMap p = getAny . foldMap (Any . p)

-- Проверка, удовлетворяют ли все элементы списка предикату
allFoldMap :: (a -> Bool) -> [a] -> Bool
allFoldMap p = getAll . foldMap (All . p)

-- Часть 4: Практические примеры использования свёртки

-- Пример 1: Подсчет частоты элементов в списке
frequency :: Ord a => [a] -> [(a, Int)]
frequency = foldr (\x acc -> updateCount x acc) []
  where
    updateCount x [] = [(x, 1)]
    updateCount x ((y, n):ys)
      | x == y    = (y, n + 1) : ys
      | otherwise = (y, n) : updateCount x ys

-- Простая реализация Map для демонстрации
type SimpleMap k v = [(k, v)]

-- Более эффективная версия с использованием foldl'
frequency' :: Ord a => [a] -> [(a, Int)]
frequency' xs = toList $ foldl' (\acc x -> insertWith (+) x 1 acc) empty xs
  where
    empty = [] :: SimpleMap a Int
    insertWith f k v [] = [(k, v)]
    insertWith f k v ((k', v'):rest)
      | k == k'   = (k', f v v') : rest
      | otherwise = (k', v') : insertWith f k v rest
    toList = id

-- Пример 2: Группировка элементов списка по результату применения функции
groupBy' :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupBy' f = foldr (\x acc -> updateGroup (f x) x acc) []
  where
    updateGroup key x [] = [(key, [x])]
    updateGroup key x ((key', xs):rest)
      | key == key' = (key', x:xs) : rest
      | otherwise   = (key', xs) : updateGroup key x rest

-- Пример 3: Построение дерева из списка
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- Построение сбалансированного дерева из отсортированного списка
buildTree :: [a] -> Tree a
buildTree [] = Empty
buildTree xs = fst $ foldr buildNode (Empty, 0) xs
  where
    buildNode x (tree, size) = (insertAt x size tree, size + 1)
    
    insertAt x 0 Empty = Node x Empty Empty
    insertAt x size Empty
      | size == 0 = Node x Empty Empty
      | otherwise =
          let leftSize = size `div` 2
              rightSize = size - leftSize - 1
          in Node x (fst $ foldr buildNode (Empty, 0) (take leftSize xs))
                   (fst $ foldr buildNode (Empty, 0) (drop (leftSize + 1) xs))

-- Пример 4: Реализация функции foldTree для свёртки дерева
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ z Empty = z
foldTree f z (Node x left right) = foldTree f (f x (foldTree f z right)) left

-- Сумма элементов дерева
sumTree :: Num a => Tree a -> a
sumTree = foldTree (+) 0

-- Пример 5: Парсинг выражений с помощью свёртки
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr deriving (Show, Eq)

-- Парсинг выражения из списка токенов
data Token = TLit Int | TOp Char | TLParen | TRParen deriving (Show, Eq)

parseExpr :: [Token] -> Maybe Expr
parseExpr tokens = case parse tokens of
  ([expr], []) -> Just expr
  _            -> Nothing
  where
    parse = foldl step ([], [])
    
    step (output, operators) (TLit n) = (Lit n : output, operators)
    step (output, operators) (TOp '+') = (output, '+' : operators)
    step (output, operators) (TOp '*') = (output, '*' : operators)
    step (output, operators) TLParen = (output, '(' : operators)
    step (output, operators) TRParen =
      let (newOutput, newOperators) = processUntilLeftParen (output, operators)
      in (newOutput, tail newOperators)  -- Remove '('
    
    processUntilLeftParen (output, '(' : ops) = (output, '(' : ops)
    processUntilLeftParen (x:y:output, '+' : ops) = processUntilLeftParen (Add y x : output, ops)
    processUntilLeftParen (x:y:output, '*' : ops) = processUntilLeftParen (Mul y x : output, ops)
    processUntilLeftParen (output, []) = (output, [])

-- Пример 6: Реализация функции mapAccumL с помощью foldl
mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL f acc = foldl step (acc, [])
  where
    step (acc', ys) x =
      let (acc'', y) = f acc' x
      in (acc'', ys ++ [y])

-- Пример использования mapAccumL для генерации последовательности Фибоначчи
fibonacci :: Int -> [Int]
fibonacci n = snd $ mapAccumL (\(a, b) _ -> ((b, a + b), a)) (0, 1) [1..n]

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: foldr - свёртка справа"
  
  putStrLn $ "sumFoldr [1, 2, 3, 4, 5]: " ++ show (sumFoldr [1, 2, 3, 4, 5])
  putStrLn $ "productFoldr [1, 2, 3, 4, 5]: " ++ show (productFoldr [1, 2, 3, 4, 5])
  putStrLn $ "concatFoldr [[1, 2], [3, 4], [5]]: " ++ show (concatFoldr [[1, 2], [3, 4], [5 :: Int]])

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: foldl - свёртка слева"
  
  putStrLn $ "sumFoldl [1, 2, 3, 4, 5]: " ++ show (sumFoldl [1, 2, 3, 4, 5])
  putStrLn $ "productFoldl [1, 2, 3, 4, 5]: " ++ show (productFoldl [1, 2, 3, 4, 5])
  putStrLn $ "reverseFoldl [1, 2, 3, 4, 5]: " ++ show (reverseFoldl [1, 2, 3, 4, 5 :: Int])

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Строгие версии foldl и foldr"
  
  putStrLn $ "sumFoldl' [1, 2, 3, 4, 5]: " ++ show (sumFoldl' [1, 2, 3, 4, 5])
  putStrLn $ "productFoldl' [1, 2, 3, 4, 5]: " ++ show (productFoldl' [1, 2, 3, 4, 5])

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Реализация стандартных функций с помощью свёртки"
  
  putStrLn $ "mapFoldr (*2) [1, 2, 3, 4, 5]: " ++ show (mapFoldr (*2) [1, 2, 3, 4, 5])
  putStrLn $ "filterFoldr even [1, 2, 3, 4, 5]: " ++ show (filterFoldr even [1, 2, 3, 4, 5])
  putStrLn $ "anyFoldr even [1, 3, 5, 7, 9]: " ++ show (anyFoldr even [1, 3, 5, 7, 9])
  putStrLn $ "anyFoldr even [1, 2, 3, 4, 5]: " ++ show (anyFoldr even [1, 2, 3, 4, 5])
  putStrLn $ "allFoldr even [2, 4, 6, 8, 10]: " ++ show (allFoldr even [2, 4, 6, 8, 10])
  putStrLn $ "allFoldr even [2, 4, 5, 6, 8]: " ++ show (allFoldr even [2, 4, 5, 6, 8])
  putStrLn $ "lengthFoldl [1, 2, 3, 4, 5]: " ++ show (lengthFoldl [1, 2, 3, 4, 5])
  putStrLn $ "elemFoldr 3 [1, 2, 3, 4, 5]: " ++ show (elemFoldr 3 [1, 2, 3, 4, 5])
  putStrLn $ "elemFoldr 6 [1, 2, 3, 4, 5]: " ++ show (elemFoldr 6 [1, 2, 3, 4, 5])

example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Другие виды свёртки"
  
  putStrLn $ "maximumFoldl1 [3, 1, 4, 1, 5, 9, 2, 6]: " ++ show (maximumFoldl1 [3, 1, 4, 1, 5, 9, 2, 6])
  putStrLn $ "minimumFoldl1 [3, 1, 4, 1, 5, 9, 2, 6]: " ++ show (minimumFoldl1 [3, 1, 4, 1, 5, 9, 2, 6])
  
  putStrLn $ "partialSums [1, 2, 3, 4, 5]: " ++ show (partialSums [1, 2, 3, 4, 5])
  putStrLn $ "partialProducts [1, 2, 3, 4, 5]: " ++ show (partialProducts [1, 2, 3, 4, 5])
  
  putStrLn $ "sumFoldMap [1, 2, 3, 4, 5]: " ++ show (sumFoldMap [1, 2, 3, 4, 5])
  putStrLn $ "productFoldMap [1, 2, 3, 4, 5]: " ++ show (productFoldMap [1, 2, 3, 4, 5])
  putStrLn $ "anyFoldMap even [1, 3, 5, 7, 9]: " ++ show (anyFoldMap even [1, 3, 5, 7, 9])
  putStrLn $ "anyFoldMap even [1, 2, 3, 4, 5]: " ++ show (anyFoldMap even [1, 2, 3, 4, 5])
  putStrLn $ "allFoldMap even [2, 4, 6, 8, 10]: " ++ show (allFoldMap even [2, 4, 6, 8, 10])
  putStrLn $ "allFoldMap even [2, 4, 5, 6, 8]: " ++ show (allFoldMap even [2, 4, 5, 6, 8])

example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Практические примеры использования свёртки"
  
  putStrLn $ "frequency [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]: " ++ show (frequency [1, 2, 2, 3, 3, 3, 4, 4, 4, 4])
  putStrLn $ "frequency' [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]: " ++ show (frequency' [1, 2, 2, 3, 3, 3, 4, 4, 4, 4])
  
  putStrLn $ "groupBy' (`mod` 3) [1, 2, 3, 4, 5, 6, 7, 8, 9]: " ++ show (groupBy' (`mod` 3) [1, 2, 3, 4, 5, 6, 7, 8, 9])
  
  let tree = Node 3 (Node 1 Empty (Node 2 Empty Empty)) (Node 5 (Node 4 Empty Empty) Empty)
  putStrLn $ "sumTree для дерева: " ++ show (sumTree tree)
  
  putStrLn $ "fibonacci 10: " ++ show (fibonacci 10)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Свёртка (Folding) в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о свёртке:"
  putStrLn "1. Свёртка (folding) - это процесс преобразования структуры данных в единственное значение"
  putStrLn "2. foldr - свёртка справа, которая начинает с правого конца списка"
  putStrLn "3. foldl - свёртка слева, которая начинает с левого конца списка"
  putStrLn "4. foldl' и foldr' - строгие версии foldl и foldr, которые избегают накопления отложенных вычислений"
  putStrLn "5. foldl1 и foldr1 - свёртки, которые используют первый элемент списка в качестве начального значения"
  putStrLn "6. scanl и scanr - функции, которые возвращают все промежуточные значения аккумулятора"
  putStrLn "7. foldMap - свёртка с использованием моноида"
  putStrLn "8. Многие стандартные функции могут быть реализованы с помощью свёртки"
  putStrLn "9. Свёртка может быть применена не только к спискам, но и к другим структурам данных"
  putStrLn "10. Выбор между foldr и foldl зависит от конкретной задачи и структуры данных"
