{-
  Практическое применение функторов в Haskell
  
  В этом файле мы рассмотрим практические примеры использования функторов
  для решения реальных задач программирования.
-}

module PracticalFunctors where

import Data.Char (toUpper)
import Data.List (intercalate)

-- Пример 1: Обработка данных с возможными ошибками
-- Функторы позволяют писать код, который элегантно обрабатывает ошибки

-- Функция для безопасного деления
safeDivide :: Int -> Int -> Maybe Int
safeDivide _ 0 = Nothing  -- Деление на ноль
safeDivide x y = Just (x `div` y)

-- Функция для безопасного извлечения корня
safeSqrt :: Int -> Maybe Int
safeSqrt x
  | x < 0 = Nothing  -- Корень из отрицательного числа
  | otherwise = Just (floor (sqrt (fromIntegral x)))

-- Использование функторов для обработки результатов
errorHandlingExample :: IO ()
errorHandlingExample = do
  putStrLn "1. Обработка данных с возможными ошибками:"
  
  -- Безопасное деление
  let result1 = safeDivide 10 2
  putStrLn $ "  10 / 2 = " ++ show result1
  
  let result2 = safeDivide 10 0
  putStrLn $ "  10 / 0 = " ++ show result2
  
  -- Применение функции к результату с использованием функтора
  let doubled1 = fmap (*2) result1
  putStrLn $ "  (10 / 2) * 2 = " ++ show doubled1
  
  let doubled2 = fmap (*2) result2
  putStrLn $ "  (10 / 0) * 2 = " ++ show doubled2
  
  -- Цепочка вычислений с возможными ошибками
  let result3 = fmap safeSqrt (safeDivide 16 4)  -- Just (Just 2)
  putStrLn $ "  sqrt(16 / 4) = " ++ show result3
  
  -- Для упрощения работы с вложенными Maybe можно использовать монады,
  -- но это тема для другого урока

-- Пример 2: Преобразование данных в коллекциях
-- Функторы позволяют легко преобразовывать данные в коллекциях

-- Тип данных для представления пользователя
data User = User {
  name :: String,
  age :: Int,
  email :: String
} deriving Show

-- Список пользователей
users :: [User]
users = [
  User "Alice" 25 "alice@example.com",
  User "Bob" 30 "bob@example.com",
  User "Charlie" 35 "charlie@example.com"
  ]

-- Функции для преобразования пользователей
incrementAge :: User -> User
incrementAge user = user { age = age user + 1 }

uppercaseName :: User -> User
uppercaseName user = user { name = map toUpper (name user) }

-- Использование функторов для преобразования данных
dataTransformationExample :: IO ()
dataTransformationExample = do
  putStrLn "\n2. Преобразование данных в коллекциях:"
  
  putStrLn "  Исходные пользователи:"
  mapM_ (putStrLn . ("    " ++) . show) users
  
  -- Увеличение возраста всех пользователей
  let olderUsers = fmap incrementAge users
  putStrLn "\n  Пользователи с увеличенным возрастом:"
  mapM_ (putStrLn . ("    " ++) . show) olderUsers
  
  -- Преобразование имен в верхний регистр
  let uppercaseUsers = fmap uppercaseName users
  putStrLn "\n  Пользователи с именами в верхнем регистре:"
  mapM_ (putStrLn . ("    " ++) . show) uppercaseUsers
  
  -- Комбинирование преобразований
  let transformedUsers = fmap (uppercaseName . incrementAge) users
  putStrLn "\n  Пользователи после обоих преобразований:"
  mapM_ (putStrLn . ("    " ++) . show) transformedUsers

-- Пример 3: Функциональные конфигурации
-- Функторы позволяют создавать гибкие конфигурации для приложений

-- Тип данных для конфигурации
data Config = Config {
  serverHost :: String,
  serverPort :: Int,
  maxConnections :: Int,
  timeout :: Int
} deriving Show

-- Значение конфигурации по умолчанию
defaultConfig :: Config
defaultConfig = Config {
  serverHost = "localhost",
  serverPort = 8080,
  maxConnections = 10,
  timeout = 30
}

-- Функции для модификации конфигурации
setProductionHost :: Config -> Config
setProductionHost config = config { serverHost = "example.com" }

increaseConnections :: Int -> Config -> Config
increaseConnections n config = config { maxConnections = maxConnections config + n }

setTimeout :: Int -> Config -> Config
setTimeout t config = config { timeout = t }

-- Использование функторов для создания конфигураций
configurationExample :: IO ()
configurationExample = do
  putStrLn "\n3. Функциональные конфигурации:"
  
  putStrLn $ "  Конфигурация по умолчанию:\n    " ++ show defaultConfig
  
  -- Создание конфигурации для разработки
  let devConfig = defaultConfig
  putStrLn $ "\n  Конфигурация для разработки:\n    " ++ show devConfig
  
  -- Создание конфигурации для тестирования
  let testConfig = setTimeout 60 $ increaseConnections 5 defaultConfig
  putStrLn $ "\n  Конфигурация для тестирования:\n    " ++ show testConfig
  
  -- Создание конфигурации для продакшена
  let prodConfig = setTimeout 120 $ increaseConnections 20 $ setProductionHost defaultConfig
  putStrLn $ "\n  Конфигурация для продакшена:\n    " ++ show prodConfig
  
  -- Функтор для функций с фиксированным аргументом
  -- Мы можем использовать функтор ((->) Config) для создания конфигураций
  let configTransformation = setProductionHost . increaseConnections 20 . setTimeout 120
  let prodConfig2 = configTransformation defaultConfig
  putStrLn $ "\n  Конфигурация для продакшена (с использованием композиции):\n    " ++ show prodConfig2

-- Пример 4: Обработка ввода-вывода
-- Функторы позволяют преобразовывать результаты операций ввода-вывода

-- Функция для чтения и преобразования ввода
ioExample :: IO ()
ioExample = do
  putStrLn "\n4. Обработка ввода-вывода:"
  
  -- Создаем IO-действие, которое возвращает строку
  let action = return "Hello, World!" :: IO String
  
  -- Применяем функцию к результату IO-действия
  let uppercaseAction = fmap (map toUpper) action
  
  -- Выполняем действие и получаем результат
  result <- uppercaseAction
  putStrLn $ "  Результат: " ++ result
  
  -- Пример с чтением ввода (закомментирован для автоматического запуска)
  putStrLn "  Для интерактивного примера раскомментируйте код в исходном файле"
  
  -- Раскомментируйте для интерактивного примера:
  -- putStr "  Введите ваше имя: "
  -- name <- fmap (map toUpper) getLine
  -- putStrLn $ "  Привет, " ++ name ++ "!"

-- Пример 5: Парсинг и валидация данных
-- Функторы полезны при парсинге и валидации данных

-- Тип данных для результата парсинга
data ParseResult a = 
    Success a
  | Error String
  deriving Show

-- Реализация функтора для ParseResult
instance Functor ParseResult where
  fmap f (Success a) = Success (f a)
  fmap _ (Error msg) = Error msg

-- Функции для парсинга
parseInt :: String -> ParseResult Int
parseInt s = case reads s of
  [(n, "")] -> Success n
  _ -> Error $ "Не удалось распарсить целое число: " ++ s

parseDouble :: String -> ParseResult Double
parseDouble s = case reads s of
  [(n, "")] -> Success n
  _ -> Error $ "Не удалось распарсить число с плавающей точкой: " ++ s

-- Функция для валидации
validatePositive :: Int -> ParseResult Int
validatePositive n
  | n > 0 = Success n
  | otherwise = Error "Число должно быть положительным"

-- Использование функторов для парсинга и валидации
parsingExample :: IO ()
parsingExample = do
  putStrLn "\n5. Парсинг и валидация данных:"
  
  -- Парсинг целого числа
  let result1 = parseInt "42"
  putStrLn $ "  Парсинг \"42\": " ++ show result1
  
  -- Парсинг некорректного ввода
  let result2 = parseInt "not a number"
  putStrLn $ "  Парсинг \"not a number\": " ++ show result2
  
  -- Применение функции к результату парсинга
  let doubled = fmap (*2) result1
  putStrLn $ "  Удвоение результата: " ++ show doubled
  
  -- Цепочка парсинга и валидации
  let validated = fmap validatePositive (parseInt "42")
  putStrLn $ "  Парсинг и валидация \"42\": " ++ show validated
  
  -- Парсинг и валидация отрицательного числа
  let validated2 = fmap validatePositive (parseInt "-10")
  putStrLn $ "  Парсинг и валидация \"-10\": " ++ show validated2

-- Пример 6: Композиция функторов
-- Функторы можно комбинировать для создания более сложных структур данных

-- Функция для композиции двух функторов
composeFunctors :: IO ()
composeFunctors = do
  putStrLn "\n6. Композиция функторов:"
  
  -- Композиция Maybe и List
  let maybeList = Just [1, 2, 3] :: Maybe [Int]
  putStrLn $ "  Maybe [Int]: " ++ show maybeList
  
  -- Применение функции к внутреннему списку
  let doubledList = fmap (fmap (*2)) maybeList
  putStrLn $ "  fmap (fmap (*2)) на Maybe [Int]: " ++ show doubledList
  
  -- Композиция Either и List
  let eitherList = Right [1, 2, 3] :: Either String [Int]
  putStrLn $ "  Either String [Int]: " ++ show eitherList
  
  -- Применение функции к внутреннему списку
  let doubledEitherList = fmap (fmap (*2)) eitherList
  putStrLn $ "  fmap (fmap (*2)) на Either String [Int]: " ++ show doubledEitherList
  
  -- Композиция трех функторов
  let nestedList = [Just 1, Nothing, Just 3] :: [Maybe Int]
  putStrLn $ "  [Maybe Int]: " ++ show nestedList
  
  -- Применение функции к самому внутреннему значению
  let doubledNestedList = fmap (fmap (*2)) nestedList
  putStrLn $ "  fmap (fmap (*2)) на [Maybe Int]: " ++ show doubledNestedList

-- Пример 7: Функторы в реальных приложениях
-- Примеры использования функторов в реальных приложениях

-- Тип данных для представления запроса к API
data ApiRequest a = ApiRequest {
  endpoint :: String,
  params :: [(String, String)],
  parseResponse :: String -> ParseResult a
}

-- Функтор для ApiRequest
instance Functor ApiRequest where
  fmap f request = request {
    parseResponse = fmap f . parseResponse request
  }

-- Создание запроса к API
createUserRequest :: ApiRequest User
createUserRequest = ApiRequest {
  endpoint = "/api/users",
  params = [("name", "John"), ("age", "30"), ("email", "john@example.com")],
  parseResponse = \response -> Success (User "John" 30 "john@example.com")  -- Упрощенный парсер
}

-- Использование функторов в реальных приложениях
realWorldExample :: IO ()
realWorldExample = do
  putStrLn "\n7. Функторы в реальных приложениях:"
  
  -- Запрос к API для получения пользователя
  let userRequest = createUserRequest
  putStrLn $ "  Запрос к API: " ++ endpoint userRequest
  
  -- Преобразование результата запроса
  let uppercaseUserRequest = fmap uppercaseName userRequest
  
  -- Симуляция выполнения запроса и парсинга ответа
  let response = "{\"name\":\"John\",\"age\":30,\"email\":\"john@example.com\"}"
  let result = parseResponse userRequest response
  let uppercaseResult = parseResponse uppercaseUserRequest response
  
  putStrLn $ "  Результат запроса: " ++ show result
  putStrLn $ "  Результат запроса с преобразованием: " ++ show uppercaseResult
  
  -- Объяснение
  putStrLn "  В реальном приложении функторы позволяют:"
  putStrLn "  - Отделить логику запроса от логики обработки результата"
  putStrLn "  - Комбинировать различные преобразования результатов"
  putStrLn "  - Создавать переиспользуемые компоненты для работы с данными"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Практическое применение функторов в Haskell\n"
  
  errorHandlingExample
  dataTransformationExample
  configurationExample
  ioExample
  parsingExample
  composeFunctors
  realWorldExample
  
  putStrLn "\nКлючевые моменты о практическом применении функторов:"
  putStrLn "1. Функторы позволяют элегантно обрабатывать ошибки и отсутствующие значения"
  putStrLn "2. Функторы упрощают преобразование данных в коллекциях"
  putStrLn "3. Функторы помогают создавать гибкие конфигурации для приложений"
  putStrLn "4. Функторы позволяют преобразовывать результаты операций ввода-вывода"
  putStrLn "5. Функторы полезны при парсинге и валидации данных"
  putStrLn "6. Функторы можно комбинировать для создания более сложных структур данных"
  putStrLn "7. Функторы широко используются в реальных приложениях для обработки данных"
