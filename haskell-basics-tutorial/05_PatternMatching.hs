{-
  Сопоставление с образцом в Haskell
  
  В этом файле мы рассмотрим сопоставление с образцом (pattern matching),
  которое является одним из ключевых механизмов в Haskell для разбора
  структур данных и определения поведения функций.
-}

module Main where

-- Сопоставление с образцом для простых значений

-- Определение функции через сопоставление с образцом
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- Функция, которая возвращает текстовое представление числа от 1 до 5
numberToString :: Int -> String
numberToString 1 = "один"
numberToString 2 = "два"
numberToString 3 = "три"
numberToString 4 = "четыре"
numberToString 5 = "пять"
numberToString _ = "число вне диапазона 1-5"

-- Сопоставление с образцом для кортежей (tuples)

-- Функция, которая извлекает первый элемент из пары
first :: (a, b) -> a
first (x, _) = x

-- Функция, которая извлекает второй элемент из пары
second :: (a, b) -> b
second (_, y) = y

-- Функция, которая меняет местами элементы пары
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Функция, которая вычисляет расстояние между двумя точками на плоскости
distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Сопоставление с образцом для списков

-- Функция, которая проверяет, пуст ли список
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

-- Функция, которая возвращает первый элемент списка (head)
myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

-- Функция, которая возвращает все элементы списка, кроме первого (tail)
myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

-- Функция, которая возвращает длину списка
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Функция, которая суммирует элементы списка чисел
mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- Функция, которая объединяет два списка (++)
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- Сопоставление с образцом для алгебраических типов данных

-- Определение типа данных для представления фигур
data Shape = Circle Double           -- Круг с радиусом
           | Rectangle Double Double -- Прямоугольник с шириной и высотой
           | Triangle Double Double Double -- Треугольник с тремя сторонами
  deriving (Show, Eq)

-- Функция для вычисления площади фигуры
area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Triangle a b c) = 
  let s = (a + b + c) / 2
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- Функция для вычисления периметра фигуры
perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Rectangle w h) = 2 * (w + h)
perimeter (Triangle a b c) = a + b + c

-- Определение типа данных для представления выражений
data Expr = Lit Int                -- Литерал (число)
          | Add Expr Expr          -- Сложение
          | Sub Expr Expr          -- Вычитание
          | Mul Expr Expr          -- Умножение
          | Div Expr Expr          -- Деление
  deriving (Show, Eq)

-- Функция для вычисления значения выражения
eval :: Expr -> Maybe Int
eval (Lit n) = Just n
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)
eval (Sub e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 - v2)
eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)
eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  if v2 == 0
    then Nothing
    else Just (v1 `div` v2)

-- Сопоставление с образцом с охранными выражениями (guards)

-- Функция, которая определяет знак числа
sign :: Int -> String
sign n
  | n < 0     = "отрицательное"
  | n > 0     = "положительное"
  | otherwise = "ноль"

-- Функция, которая определяет возрастную категорию
ageCategory :: Int -> String
ageCategory age
  | age < 0     = "некорректный возраст"
  | age < 18    = "несовершеннолетний"
  | age < 65    = "взрослый"
  | otherwise   = "пожилой"

-- Функция, которая определяет оценку по баллам
grade :: Int -> String
grade score
  | score < 0   = "некорректный балл"
  | score < 60  = "неудовлетворительно"
  | score < 75  = "удовлетворительно"
  | score < 90  = "хорошо"
  | score <= 100 = "отлично"
  | otherwise   = "некорректный балл"

-- Сопоставление с образцом с использованием as-паттернов (@)

-- Функция, которая возвращает первый элемент и весь список
headAndList :: [a] -> Maybe (a, [a])
headAndList [] = Nothing
headAndList xs@(x:_) = Just (x, xs)

-- Функция, которая проверяет, начинается ли список с определенного элемента
startsWith :: Eq a => a -> [a] -> Bool
startsWith _ [] = False
startsWith y xs@(x:_) = y == x

-- Сопоставление с образцом с использованием вложенных паттернов

-- Функция, которая извлекает первый элемент из списка пар
firstOfPairs :: [(a, b)] -> Maybe a
firstOfPairs [] = Nothing
firstOfPairs ((x, _):_) = Just x

-- Функция, которая суммирует все элементы вложенного списка
sumNested :: [[Int]] -> Int
sumNested [] = 0
sumNested (xs:xss) = sum xs + sumNested xss

-- Пример 1: Сопоставление с образцом для простых значений
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Сопоставление с образцом для простых значений"
  
  putStrLn $ "isZero 0: " ++ show (isZero 0)
  putStrLn $ "isZero 5: " ++ show (isZero 5)
  
  putStrLn $ "numberToString 1: " ++ numberToString 1
  putStrLn $ "numberToString 3: " ++ numberToString 3
  putStrLn $ "numberToString 7: " ++ numberToString 7

-- Пример 2: Сопоставление с образцом для кортежей
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Сопоставление с образцом для кортежей"
  
  let pair = (10, "Hello")
  putStrLn $ "Пара: " ++ show pair
  putStrLn $ "Первый элемент: " ++ show (first pair)
  putStrLn $ "Второй элемент: " ++ second pair
  putStrLn $ "После swap: " ++ show (swap pair)
  
  let point1 = (0.0, 0.0)
  let point2 = (3.0, 4.0)
  putStrLn $ "Расстояние между " ++ show point1 ++ " и " ++ show point2 ++ ": " ++ show (distance point1 point2)

-- Пример 3: Сопоставление с образцом для списков
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Сопоставление с образцом для списков"
  
  let list1 = [] :: [Int]
  let list2 = [1, 2, 3, 4, 5]
  
  putStrLn $ "list1: " ++ show list1
  putStrLn $ "list2: " ++ show list2
  
  putStrLn $ "isEmpty list1: " ++ show (isEmpty list1)
  putStrLn $ "isEmpty list2: " ++ show (isEmpty list2)
  
  putStrLn $ "myHead list1: " ++ show (myHead list1 :: Maybe Int)
  putStrLn $ "myHead list2: " ++ show (myHead list2)
  
  putStrLn $ "myTail list1: " ++ show (myTail list1)
  putStrLn $ "myTail list2: " ++ show (myTail list2)
  
  putStrLn $ "myLength list1: " ++ show (myLength list1)
  putStrLn $ "myLength list2: " ++ show (myLength list2)
  
  putStrLn $ "mySum list1: " ++ show (mySum list1)
  putStrLn $ "mySum list2: " ++ show (mySum list2)
  
  putStrLn $ "myAppend list1 list2: " ++ show (myAppend list1 list2)
  putStrLn $ "myAppend list2 list1: " ++ show (myAppend list2 list1)

-- Пример 4: Сопоставление с образцом для алгебраических типов данных
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Сопоставление с образцом для алгебраических типов данных"
  
  let circle = Circle 5.0
  let rectangle = Rectangle 4.0 6.0
  let triangle = Triangle 3.0 4.0 5.0
  
  putStrLn $ "Площадь круга: " ++ show (area circle)
  putStrLn $ "Площадь прямоугольника: " ++ show (area rectangle)
  putStrLn $ "Площадь треугольника: " ++ show (area triangle)
  
  putStrLn $ "Периметр круга: " ++ show (perimeter circle)
  putStrLn $ "Периметр прямоугольника: " ++ show (perimeter rectangle)
  putStrLn $ "Периметр треугольника: " ++ show (perimeter triangle)
  
  let expr1 = Add (Lit 5) (Lit 3)
  let expr2 = Mul (Lit 4) (Sub (Lit 7) (Lit 2))
  let expr3 = Div (Lit 10) (Lit 0)
  
  putStrLn $ "Выражение: " ++ show expr1 ++ " = " ++ show (eval expr1)
  putStrLn $ "Выражение: " ++ show expr2 ++ " = " ++ show (eval expr2)
  putStrLn $ "Выражение: " ++ show expr3 ++ " = " ++ show (eval expr3)

-- Пример 5: Сопоставление с образцом с охранными выражениями
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Сопоставление с образцом с охранными выражениями"
  
  putStrLn $ "sign (-5): " ++ sign (-5)
  putStrLn $ "sign 0: " ++ sign 0
  putStrLn $ "sign 10: " ++ sign 10
  
  putStrLn $ "ageCategory 15: " ++ ageCategory 15
  putStrLn $ "ageCategory 30: " ++ ageCategory 30
  putStrLn $ "ageCategory 70: " ++ ageCategory 70
  
  putStrLn $ "grade 50: " ++ grade 50
  putStrLn $ "grade 70: " ++ grade 70
  putStrLn $ "grade 85: " ++ grade 85
  putStrLn $ "grade 95: " ++ grade 95

-- Пример 6: Сопоставление с образцом с использованием as-паттернов и вложенных паттернов
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Сопоставление с образцом с использованием as-паттернов и вложенных паттернов"
  
  let list = [1, 2, 3, 4, 5]
  
  putStrLn $ "headAndList []: " ++ show (headAndList ([] :: [Int]) :: Maybe (Int, [Int]))
  putStrLn $ "headAndList " ++ show list ++ ": " ++ show (headAndList list :: Maybe (Int, [Int]))
  
  putStrLn $ "startsWith 1 " ++ show list ++ ": " ++ show (startsWith 1 list)
  putStrLn $ "startsWith 2 " ++ show list ++ ": " ++ show (startsWith 2 list)
  
  let pairsList = [(1, "one"), (2, "two"), (3, "three")]
  putStrLn $ "firstOfPairs []: " ++ show (firstOfPairs ([] :: [(Int, String)]) :: Maybe Int)
  putStrLn $ "firstOfPairs " ++ show pairsList ++ ": " ++ show (firstOfPairs pairsList :: Maybe Int)
  
  let nestedList = [[1, 2], [3, 4], [5, 6]]
  putStrLn $ "sumNested " ++ show nestedList ++ ": " ++ show (sumNested nestedList)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Сопоставление с образцом в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о сопоставлении с образцом:"
  putStrLn "1. Сопоставление с образцом позволяет определять функции через разбор структуры аргументов"
  putStrLn "2. Образцы проверяются сверху вниз, используется первый подходящий образец"
  putStrLn "3. Символ подчеркивания (_) используется как шаблон, который соответствует любому значению"
  putStrLn "4. Сопоставление с образцом можно использовать для простых значений, кортежей, списков и алгебраических типов данных"
  putStrLn "5. Охранные выражения (guards) позволяют добавлять условия к образцам"
  putStrLn "6. As-паттерны (@) позволяют связать имя с целым значением, соответствующим образцу"
  putStrLn "7. Вложенные паттерны позволяют сопоставлять с образцом вложенные структуры данных"
  putStrLn "8. Сопоставление с образцом делает код более читаемым и выразительным"
