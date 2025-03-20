{-
  Стандартные функторы в Haskell
  
  В этом файле мы рассмотрим наиболее распространенные функторы,
  которые встречаются в стандартной библиотеке Haskell.
-}

module CommonFunctors where

import Data.Char (toUpper)

-- Напомним определение типового класса Functor:
-- 
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- Оператор <$> - это инфиксная версия fmap:
-- f <$> x = fmap f x

-- 1. Maybe Functor
-- Maybe представляет вычисление, которое может завершиться неудачей
-- 
-- instance Functor Maybe where
--   fmap _ Nothing = Nothing
--   fmap f (Just x) = Just (f x)

maybeExample :: IO ()
maybeExample = do
  putStrLn "1. Функтор Maybe:"
  
  -- Применение функции к Just-значению
  let justValue = Just 42
  let doubledJust = fmap (*2) justValue
  putStrLn $ "  Just 42 -> fmap (*2) -> " ++ show doubledJust
  
  -- Применение функции к Nothing
  let nothingValue = Nothing :: Maybe Int
  let doubledNothing = fmap (*2) nothingValue
  putStrLn $ "  Nothing -> fmap (*2) -> " ++ show doubledNothing
  
  -- Цепочка преобразований с Maybe
  let result = fmap show (fmap (*3) justValue)
  putStrLn $ "  Цепочка преобразований: " ++ show result
  
  -- Практический пример: безопасное деление
  let safeDivide x y = if y == 0 then Nothing else Just (x `div` y)
  let divResult1 = fmap (*2) (safeDivide 10 2)
  let divResult2 = fmap (*2) (safeDivide 10 0)
  putStrLn $ "  10 / 2 * 2 = " ++ show divResult1
  putStrLn $ "  10 / 0 * 2 = " ++ show divResult2

-- 2. List Functor
-- Список - это контейнер, который может содержать несколько значений
-- 
-- instance Functor [] where
--   fmap = map

listExample :: IO ()
listExample = do
  putStrLn "\n2. Функтор списка:"
  
  -- Применение функции к каждому элементу списка
  let numbers = [1, 2, 3, 4, 5]
  let doubled = fmap (*2) numbers
  putStrLn $ "  [1,2,3,4,5] -> fmap (*2) -> " ++ show doubled
  
  -- Работа с пустым списком
  let emptyList = [] :: [Int]
  let doubledEmpty = fmap (*2) emptyList
  putStrLn $ "  [] -> fmap (*2) -> " ++ show doubledEmpty
  
  -- Применение к списку строк
  let words = ["hello", "world", "haskell"]
  let uppercased = fmap (map toUpper) words
  putStrLn $ "  " ++ show words ++ " -> fmap (map toUpper) -> " ++ show uppercased
  
  -- Вложенные списки
  let nested = [[1, 2], [3, 4], [5, 6]]
  let sumLists = fmap sum nested
  putStrLn $ "  " ++ show nested ++ " -> fmap sum -> " ++ show sumLists

-- 3. Either Functor
-- Either представляет вычисление, которое может завершиться ошибкой с информацией о ней
-- 
-- instance Functor (Either a) where
--   fmap _ (Left x) = Left x
--   fmap f (Right y) = Right (f y)
--
-- Обратите внимание, что Either a - это частично примененный тип-конструктор,
-- где a - это тип левого значения (обычно ошибки)

eitherExample :: IO ()
eitherExample = do
  putStrLn "\n3. Функтор Either:"
  
  -- Применение функции к Right-значению
  let rightValue = Right 42 :: Either String Int
  let doubledRight = fmap (*2) rightValue
  putStrLn $ "  Right 42 -> fmap (*2) -> " ++ show doubledRight
  
  -- Применение функции к Left-значению
  let leftValue = Left "error" :: Either String Int
  let doubledLeft = fmap (*2) leftValue
  putStrLn $ "  Left \"error\" -> fmap (*2) -> " ++ show doubledLeft
  
  -- Практический пример: обработка ошибок
  let safeDivide x y = if y == 0 
                        then Left "Division by zero" 
                        else Right (x `div` y)
  let divResult1 = fmap (*2) (safeDivide 10 2)
  let divResult2 = fmap (*2) (safeDivide 10 0)
  putStrLn $ "  10 / 2 * 2 = " ++ show divResult1
  putStrLn $ "  10 / 0 * 2 = " ++ show divResult2

-- 4. IO Functor
-- IO представляет вычисление с побочными эффектами
-- 
-- instance Functor IO where
--   fmap f action = do
--     result <- action
--     return (f result)

ioExample :: IO ()
ioExample = do
  putStrLn "\n4. Функтор IO:"
  
  -- Простой пример с return
  let ioValue = return 42 :: IO Int
  putStrLn "  Создаем IO Int со значением 42"
  
  -- Применяем функцию к IO-значению
  putStrLn "  Применяем (*2) к IO-значению"
  result <- fmap (*2) ioValue
  putStrLn $ "  Результат: " ++ show result
  
  -- Практический пример: чтение и преобразование ввода
  putStrLn "  Для демонстрации преобразования ввода раскомментируйте код в исходном файле"
  
  -- Раскомментируйте для интерактивного примера:
  -- putStr "  Введите число: "
  -- input <- fmap read getLine :: IO Int
  -- let doubled = fmap (*2) (return input)
  -- result <- doubled
  -- putStrLn $ "  Удвоенное число: " ++ show result

-- 5. ((->) r) Functor
-- Функции от типа r также образуют функтор
-- 
-- instance Functor ((->) r) where
--   fmap f g = f . g
--
-- Здесь fmap - это просто композиция функций!

functionExample :: IO ()
functionExample = do
  putStrLn "\n5. Функтор ((->) r):"
  
  -- Определяем базовую функцию
  let addOne = (+1) :: Int -> Int
  putStrLn $ "  addOne 5 = " ++ show (addOne 5)
  
  -- Применяем функцию к результату другой функции
  let composed = fmap (*2) addOne
  putStrLn $ "  fmap (*2) addOne = (*2) . addOne"
  putStrLn $ "  composed 5 = " ++ show (composed 5)
  
  -- Эквивалентно композиции функций
  let composed2 = (*2) . addOne
  putStrLn $ "  composed2 5 = " ++ show (composed2 5)
  
  -- Цепочка преобразований
  let chain = fmap show (fmap (*3) addOne)
  putStrLn $ "  fmap show (fmap (*3) addOne) 5 = " ++ show (chain 5)

-- 6. Tuple Functor
-- Пары (и кортежи вообще) также являются функторами, но только по второму элементу
-- 
-- instance Functor ((,) a) where
--   fmap f (x, y) = (x, f y)
--
-- Обратите внимание, что ((,) a) - это частично примененный тип-конструктор,
-- где a - это тип первого элемента пары

tupleExample :: IO ()
tupleExample = do
  putStrLn "\n6. Функтор ((,) a) (пары):"
  
  -- Определяем пару
  let pair = (42, "hello")
  putStrLn $ "  Исходная пара: " ++ show pair
  
  -- Применяем функцию ко второму элементу пары
  let mappedPair = fmap reverse pair
  putStrLn $ "  fmap reverse (42, \"hello\") = " ++ show mappedPair
  
  -- Нельзя применить функцию к первому элементу напрямую
  putStrLn "  Нельзя применить функцию к первому элементу напрямую"
  putStrLn "  Для этого нужно использовать другие средства, например, bimap из Data.Bifunctor"
  
  -- Практический пример: пары как помеченные значения
  let users = [("Alice", 25), ("Bob", 30), ("Charlie", 35)]
  let incrementAge = fmap (+1)
  let updatedUsers = map incrementAge users
  putStrLn $ "  Исходные пользователи: " ++ show users
  putStrLn $ "  После увеличения возраста: " ++ show updatedUsers

-- 7. Другие стандартные функторы
otherFunctorsExample :: IO ()
otherFunctorsExample = do
  putStrLn "\n7. Другие стандартные функторы:"
  
  -- Identity - простейший функтор, просто содержит значение
  putStrLn "  Identity a - просто содержит значение типа a"
  putStrLn "  instance Functor Identity where"
  putStrLn "    fmap f (Identity x) = Identity (f x)"
  
  -- Const - функтор, который игнорирует применяемую функцию
  putStrLn "\n  Const a b - содержит значение типа a и игнорирует тип b"
  putStrLn "  instance Functor (Const a) where"
  putStrLn "    fmap _ (Const x) = Const x"
  
  -- Tree - пример функтора для древовидной структуры
  putStrLn "\n  Tree - пример функтора для древовидной структуры"
  putStrLn "  data Tree a = Leaf a | Node (Tree a) (Tree a)"
  putStrLn "  instance Functor Tree where"
  putStrLn "    fmap f (Leaf x) = Leaf (f x)"
  putStrLn "    fmap f (Node left right) = Node (fmap f left) (fmap f right)"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Стандартные функторы в Haskell\n"
  
  maybeExample
  listExample
  eitherExample
  ioExample
  functionExample
  tupleExample
  otherFunctorsExample
  
  putStrLn "\nКлючевые моменты о стандартных функторах:"
  putStrLn "1. Maybe - функтор для вычислений, которые могут завершиться неудачей"
  putStrLn "2. [] (список) - функтор для коллекций значений"
  putStrLn "3. Either a - функтор для вычислений с информативными ошибками"
  putStrLn "4. IO - функтор для вычислений с побочными эффектами"
  putStrLn "5. ((->) r) - функтор для функций, fmap - это композиция функций"
  putStrLn "6. ((,) a) - функтор для пар, применяет функцию только ко второму элементу"
  putStrLn "7. Существует множество других функторов в стандартной библиотеке и пакетах"
