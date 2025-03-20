{-
  Введение в функторы в Haskell
  
  Функтор - это абстракция, которая представляет собой контейнер или контекст,
  к содержимому которого можно применить функцию.
-}

module IntroToFunctors where

-- Типовой класс Functor определен в стандартной библиотеке Haskell следующим образом:
-- 
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- Здесь:
-- - f - это тип-конструктор с одним параметром (например, Maybe, [], IO)
-- - fmap - это функция, которая принимает функцию (a -> b) и контейнер f a,
--   и возвращает контейнер f b, применяя функцию к содержимому контейнера

-- Оператор <$> - это инфиксная версия fmap
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- f <$> x = fmap f x

-- Пример 1: Функтор Maybe
-- Maybe представляет вычисление, которое может завершиться неудачей

-- Применение функции к Maybe-значению
applyToMaybe :: (a -> b) -> Maybe a -> Maybe b
applyToMaybe _ Nothing = Nothing  -- Если значение отсутствует, результат тоже отсутствует
applyToMaybe f (Just x) = Just (f x)  -- Если значение есть, применяем к нему функцию

-- Это точно то же самое, что делает fmap для Maybe:
-- instance Functor Maybe where
--   fmap _ Nothing = Nothing
--   fmap f (Just x) = Just (f x)

-- Пример использования fmap с Maybe
maybeExample :: IO ()
maybeExample = do
  putStrLn "Пример функтора Maybe:"
  
  let maybeInt = Just 5
  putStrLn $ "  Исходное значение: " ++ show maybeInt
  
  let doubled = fmap (*2) maybeInt
  putStrLn $ "  После fmap (*2): " ++ show doubled
  
  let nothing = Nothing :: Maybe Int
  putStrLn $ "  fmap с Nothing: " ++ show (fmap (*2) nothing)
  
  -- Использование оператора <$>
  let squared = (^2) <$> maybeInt
  putStrLn $ "  Использование оператора <$>: " ++ show squared

-- Пример 2: Функтор списка (List)
-- Список - это контейнер, который может содержать несколько значений

-- Применение функции к каждому элементу списка
applyToList :: (a -> b) -> [a] -> [b]
applyToList f xs = [f x | x <- xs]  -- Используем list comprehension

-- Это эквивалентно определению fmap для списков:
-- instance Functor [] where
--   fmap = map

-- Пример использования fmap со списками
listExample :: IO ()
listExample = do
  putStrLn "\nПример функтора списка:"
  
  let numbers = [1, 2, 3, 4, 5]
  putStrLn $ "  Исходный список: " ++ show numbers
  
  let doubled = fmap (*2) numbers
  putStrLn $ "  После fmap (*2): " ++ show doubled
  
  let squared = (^2) <$> numbers
  putStrLn $ "  Квадраты чисел: " ++ show squared
  
  let strings = ["hello", "world", "haskell"]
  let lengths = length <$> strings
  putStrLn $ "  Длины строк: " ++ show lengths

-- Пример 3: Функтор IO
-- IO представляет вычисление с побочными эффектами

-- Пример использования fmap с IO
ioExample :: IO ()
ioExample = do
  putStrLn "\nПример функтора IO:"
  
  -- Получаем строку от пользователя
  putStr "  Введите число: "
  input <- getLine
  
  -- Используем fmap для преобразования IO String в IO Int
  let ioInt = fmap read (return input) :: IO Int
  
  -- Используем <$> для преобразования IO Int в IO String
  let ioResult = show <$> fmap (*2) ioInt
  
  -- Выводим результат
  result <- ioResult
  putStrLn $ "  Удвоенное число: " ++ result

-- Пример 4: Композиция функций с fmap
compositionExample :: IO ()
compositionExample = do
  putStrLn "\nПример композиции функций с fmap:"
  
  let maybeInt = Just 5
  putStrLn $ "  Исходное значение: " ++ show maybeInt
  
  -- Последовательное применение fmap
  let result1 = fmap (*2) (fmap (+3) maybeInt)
  putStrLn $ "  fmap (*2) (fmap (+3) value): " ++ show result1
  
  -- Композиция функций внутри fmap
  let result2 = fmap ((*2) . (+3)) maybeInt
  putStrLn $ "  fmap ((*2) . (+3)) value: " ++ show result2
  
  -- Эти два выражения эквивалентны благодаря законам функторов

-- Пример 5: Функтор для функций ((->) r)
-- Функции от типа r также образуют функтор
-- instance Functor ((->) r) where
--   fmap f g = f . g

functionExample :: IO ()
functionExample = do
  putStrLn "\nПример функтора для функций:"
  
  let addOne = (+1)
  let double = (*2)
  
  -- fmap для функций - это композиция функций
  let composed = fmap double addOne
  
  putStrLn $ "  addOne 5: " ++ show (addOne 5)
  putStrLn $ "  double 5: " ++ show (double 5)
  putStrLn $ "  (fmap double addOne) 5: " ++ show (composed 5)
  putStrLn $ "  Это эквивалентно (double . addOne) 5: " ++ show ((double . addOne) 5)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Введение в функторы в Haskell\n"
  
  maybeExample
  listExample
  
  -- Раскомментируйте для интерактивного примера
  -- ioExample
  
  compositionExample
  functionExample
  
  putStrLn "\nКлючевые моменты о функторах:"
  putStrLn "1. Функтор - это контейнер или контекст, к содержимому которого можно применить функцию"
  putStrLn "2. Функция fmap применяет функцию к значению в контексте, сохраняя контекст"
  putStrLn "3. Оператор <$> - это инфиксная версия fmap"
  putStrLn "4. Функторы должны удовлетворять определенным законам (см. 03_FunctorLaws.hs)"
  putStrLn "5. Многие стандартные типы в Haskell являются функторами: Maybe, [], IO, ((->) r), и др."
