{-
  Типы данных и классы типов в Haskell
  
  В этом файле мы рассмотрим основные типы данных в Haskell,
  как определять собственные типы данных и как работать с классами типов.
-}

module Main where

-- Базовые типы данных в Haskell
-- Int - целые числа фиксированной длины
-- Integer - целые числа произвольной длины
-- Float - числа с плавающей точкой одинарной точности
-- Double - числа с плавающей точкой двойной точности
-- Bool - логический тип (True или False)
-- Char - символы
-- String - строки (список символов)

-- Примеры значений базовых типов
exampleInt :: Int
exampleInt = 42

exampleInteger :: Integer
exampleInteger = 123456789012345678901234567890

exampleFloat :: Float
exampleFloat = 3.14

exampleDouble :: Double
exampleDouble = 3.14159265359

exampleBool :: Bool
exampleBool = True

exampleChar :: Char
exampleChar = 'A'

exampleString :: String
exampleString = "Hello, Haskell!"

-- Функция с явной сигнатурой типа
add :: Int -> Int -> Int
add x y = x + y

-- Функция без явной сигнатуры типа (тип выводится компилятором)
multiply x y = x * y

-- Определение собственных типов данных

-- Перечисление (Enumeration)
data Color = Red | Green | Blue | Yellow | Purple
  deriving (Show, Eq, Ord)

-- Использование перечисления
favoriteColor :: Color
favoriteColor = Blue

isRed :: Color -> Bool
isRed Red = True
isRed _ = False

-- Алгебраический тип данных (Algebraic Data Type)
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

-- Записи (Records)
data Person = Person {
  name :: String,
  age :: Int,
  address :: String
} deriving (Show, Eq)

-- Создание значения записи
john :: Person
john = Person {
  name = "John Doe",
  age = 30,
  address = "123 Main St"
}

-- Доступ к полям записи
johnsName :: String
johnsName = name john

johnsAge :: Int
johnsAge = age john

-- Обновление записи (создает новую запись)
olderJohn :: Person
olderJohn = john { age = age john + 1 }

-- Параметризованные типы данных
data Maybe' a = Nothing' | Just' a
  deriving (Show, Eq)

-- Использование параметризованного типа
maybeInt :: Maybe' Int
maybeInt = Just' 42

maybeString :: Maybe' String
maybeString = Just' "Hello"

-- Рекурсивные типы данных
data List a = Empty | Cons a (List a)
  deriving (Show, Eq)

-- Создание списка
exampleList :: List Int
exampleList = Cons 1 (Cons 2 (Cons 3 Empty))

-- Функция для вычисления длины списка
listLength :: List a -> Int
listLength Empty = 0
listLength (Cons _ xs) = 1 + listLength xs

-- Классы типов

-- Класс типов Eq определяет операции == и /=
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x /= y = not (x == y)
--   x == y = not (x /= y)

-- Класс типов Ord определяет операции сравнения
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<), (<=), (>), (>=) :: a -> a -> Bool
--   max, min :: a -> a -> a

-- Класс типов Show определяет преобразование в строку
-- class Show a where
--   show :: a -> String

-- Класс типов Read определяет преобразование из строки
-- class Read a where
--   read :: String -> a

-- Определение собственного класса типа
class Describable a where
  describe :: a -> String

-- Реализация класса типа для существующих типов
instance Describable Color where
  describe Red = "Красный цвет"
  describe Green = "Зеленый цвет"
  describe Blue = "Синий цвет"
  describe Yellow = "Желтый цвет"
  describe Purple = "Фиолетовый цвет"

instance Describable Shape where
  describe (Circle r) = "Круг с радиусом " ++ show r
  describe (Rectangle w h) = "Прямоугольник с шириной " ++ show w ++ " и высотой " ++ show h
  describe (Triangle a b c) = "Треугольник со сторонами " ++ show a ++ ", " ++ show b ++ " и " ++ show c

instance Describable Person where
  describe p = name p ++ ", " ++ show (age p) ++ " лет, проживает по адресу: " ++ address p

-- Пример 1: Базовые типы данных
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Базовые типы данных"
  
  putStrLn $ "Int: " ++ show exampleInt
  putStrLn $ "Integer: " ++ show exampleInteger
  putStrLn $ "Float: " ++ show exampleFloat
  putStrLn $ "Double: " ++ show exampleDouble
  putStrLn $ "Bool: " ++ show exampleBool
  putStrLn $ "Char: " ++ show exampleChar
  putStrLn $ "String: " ++ exampleString
  
  putStrLn $ "add 5 3 = " ++ show (add 5 3)
  putStrLn $ "multiply 4 7 = " ++ show (multiply 4 7)

-- Пример 2: Собственные типы данных
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Собственные типы данных"
  
  putStrLn $ "Любимый цвет: " ++ show favoriteColor
  putStrLn $ "Является ли Red красным? " ++ show (isRed Red)
  putStrLn $ "Является ли Blue красным? " ++ show (isRed Blue)
  
  let circle = Circle 5.0
  let rectangle = Rectangle 4.0 6.0
  let triangle = Triangle 3.0 4.0 5.0
  
  putStrLn $ "Площадь круга: " ++ show (area circle)
  putStrLn $ "Площадь прямоугольника: " ++ show (area rectangle)
  putStrLn $ "Площадь треугольника: " ++ show (area triangle)
  
  putStrLn $ "Информация о человеке: " ++ show john
  putStrLn $ "Имя: " ++ johnsName
  putStrLn $ "Возраст: " ++ show johnsAge
  putStrLn $ "Через год: " ++ show olderJohn

-- Пример 3: Параметризованные и рекурсивные типы данных
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Параметризованные и рекурсивные типы данных"
  
  putStrLn $ "Maybe Int: " ++ show maybeInt
  putStrLn $ "Maybe String: " ++ show maybeString
  
  putStrLn $ "Список: " ++ show exampleList
  putStrLn $ "Длина списка: " ++ show (listLength exampleList)

-- Пример 4: Классы типов
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Классы типов"
  
  putStrLn $ "Описание Red: " ++ describe Red
  putStrLn $ "Описание Circle 5.0: " ++ describe (Circle 5.0)
  putStrLn $ "Описание john: " ++ describe john
  
  putStrLn $ "Red == Blue: " ++ show (Red == Blue)
  putStrLn $ "Red == Red: " ++ show (Red == Red)
  
  putStrLn $ "Red < Blue (лексикографически): " ++ show (Red < Blue)
  
  putStrLn $ "show Red: " ++ show Red
  putStrLn $ "show (Circle 5.0): " ++ show (Circle 5.0)

-- Кортежи (Tuples)

-- Кортеж (tuple) - это упорядоченная коллекция элементов фиксированной длины,
-- которые могут иметь разные типы.

-- Создание кортежей
examplePair :: (Int, String)
examplePair = (42, "Hello")

exampleTriple :: (Int, String, Bool)
exampleTriple = (42, "Hello", True)

-- Кортеж из одного элемента не существует, (x) - это просто x в скобках
-- Кортеж из нуля элементов - это тип (), называемый "unit"
exampleUnit :: ()
exampleUnit = ()

-- Доступ к элементам кортежа

-- Для пар (кортежей из двух элементов) есть стандартные функции fst и snd
firstFromPair :: Int
firstFromPair = fst (42, "Hello")  -- 42

secondFromPair :: String
secondFromPair = snd (42, "Hello")  -- "Hello"

-- Для кортежей большей длины нужно использовать сопоставление с образцом
-- (подробнее о сопоставлении с образцом см. в файле 05_PatternMatching.hs)
getFirstFromTriple :: (a, b, c) -> a
getFirstFromTriple (x, _, _) = x

getSecondFromTriple :: (a, b, c) -> b
getSecondFromTriple (_, y, _) = y

getThirdFromTriple :: (a, b, c) -> c
getThirdFromTriple (_, _, z) = z

-- Использование кортежей для возврата нескольких значений из функции
findMinMax :: [Int] -> (Int, Int)
findMinMax [] = error "Пустой список"
findMinMax [x] = (x, x)
findMinMax (x:xs) = -- Здесь используется сопоставление с образцом
  let (min, max) = findMinMax xs
  in (if x < min then x else min, if x > max then x else max)

-- Реализация функций для работы с кортежами

-- Реализация функции zip
-- zip объединяет два списка в список пар
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []  -- Сопоставление с образцом
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys  -- Сопоставление с образцом для списков

-- Реализация функции unzip
-- unzip разделяет список пар на пару списков
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])  -- Сопоставление с образцом
myUnzip ((x, y):xys) = -- Сопоставление с образцом для списка пар
  let (xs, ys) = myUnzip xys
  in (x:xs, y:ys)

-- Реализация функции zipWith
-- zipWith применяет функцию к соответствующим элементам двух списков
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []  -- Сопоставление с образцом
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys  -- Сопоставление с образцом для списков

-- Реализация функции curry
-- curry преобразует функцию, принимающую пару, в функцию двух аргументов
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

-- Реализация функции uncurry
-- uncurry преобразует функцию двух аргументов в функцию, принимающую пару
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y  -- Сопоставление с образцом для пары

-- Примеры использования наших реализаций
zipExample :: [(Int, Char)]
zipExample = myZip [1, 2, 3] ['a', 'b', 'c']  -- [(1,'a'), (2,'b'), (3,'c')]

unzipExample :: ([Int], [Char])
unzipExample = myUnzip zipExample  -- ([1, 2, 3], "abc")

zipWithExample :: [Int]
zipWithExample = myZipWith (+) [1, 2, 3] [4, 5, 6]  -- [5, 7, 9]

-- Функции curry и uncurry
addPairExample :: (Int, Int) -> Int
addPairExample (x, y) = x + y  -- Сопоставление с образцом для пары

curriedAddExample :: Int -> Int -> Int
curriedAddExample = myCurry addPairExample

uncurriedAddExample :: (Int, Int) -> Int
uncurriedAddExample = myUncurry (+)

-- Пример 5: Кортежи
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Кортежи"
  
  putStrLn $ "Пара: " ++ show examplePair
  putStrLn $ "Тройка: " ++ show exampleTriple
  putStrLn $ "Unit: " ++ show exampleUnit
  
  putStrLn $ "Первый элемент пары: " ++ show firstFromPair
  putStrLn $ "Второй элемент пары: " ++ show secondFromPair
  
  putStrLn $ "Первый элемент тройки: " ++ show (getFirstFromTriple exampleTriple)
  putStrLn $ "Второй элемент тройки: " ++ show (getSecondFromTriple exampleTriple)
  putStrLn $ "Третий элемент тройки: " ++ show (getThirdFromTriple exampleTriple)
  
  let list = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  putStrLn $ "Минимум и максимум списка " ++ show list ++ ": " ++ show (findMinMax list)
  
  putStrLn $ "zip [1, 2, 3] ['a', 'b', 'c']: " ++ show zipExample
  putStrLn $ "unzip " ++ show zipExample ++ ": " ++ show unzipExample
  putStrLn $ "zipWith (+) [1, 2, 3] [4, 5, 6]: " ++ show zipWithExample
  
  putStrLn $ "addPairExample (3, 4): " ++ show (addPairExample (3, 4))
  putStrLn $ "curriedAddExample 3 4: " ++ show (curriedAddExample 3 4)
  putStrLn $ "uncurriedAddExample (3, 4): " ++ show (uncurriedAddExample (3, 4))

-- Главная функция
main :: IO ()
main = do
  putStrLn "Типы данных и классы типов в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  
  putStrLn "\nКлючевые моменты о типах данных и классах типов:"
  putStrLn "1. Haskell имеет сильную статическую систему типов с выводом типов"
  putStrLn "2. Базовые типы данных включают Int, Integer, Float, Double, Bool, Char и String"
  putStrLn "3. Собственные типы данных определяются с помощью ключевого слова data"
  putStrLn "4. Алгебраические типы данных могут иметь несколько конструкторов с разными параметрами"
  putStrLn "5. Записи предоставляют именованные поля для доступа к данным"
  putStrLn "6. Параметризованные типы данных позволяют создавать обобщенные структуры данных"
  putStrLn "7. Рекурсивные типы данных могут ссылаться на себя в своем определении"
  putStrLn "8. Классы типов определяют интерфейсы, которые могут быть реализованы разными типами"
  putStrLn "9. Стандартные классы типов включают Eq, Ord, Show, Read, Num и другие"
  putStrLn "10. Можно определять собственные классы типов и реализации для существующих типов"
  
  putStrLn "\nКлючевые моменты о кортежах:"
  putStrLn "1. Кортеж - это упорядоченная коллекция элементов фиксированной длины, которые могут иметь разные типы"
  putStrLn "2. Кортежи создаются с помощью круглых скобок и запятых: (1, \"hello\", True)"
  putStrLn "3. Тип кортежа - это кортеж типов его элементов: (Int, String, Bool)"
  putStrLn "4. Для доступа к элементам кортежа можно использовать функции fst и snd (для пар) или сопоставление с образцом"
  putStrLn "5. Кортежи часто используются для возврата нескольких значений из функции"
  putStrLn "6. Функции zip, unzip и zipWith работают с кортежами и списками"
  putStrLn "7. Функции curry и uncurry преобразуют между функциями, принимающими пару, и функциями двух аргументов"
