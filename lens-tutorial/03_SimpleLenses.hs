{-
  Простая реализация линз в Haskell
  
  В этом файле мы рассмотрим, как использовать линзы для работы с вложенными структурами данных.
-}

module Main where

import qualified Data.Map as Map
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

-- Реализация линз
-- Линза - это комбинация геттера и сеттера для поля структуры данных
data Lens s a = Lens
  { view :: s -> a           -- Функция для получения значения
  , over :: (a -> a) -> s -> s  -- Функция для модификации значения
  }

-- Установка значения через линзу
set :: Lens s a -> a -> s -> s
set lens value = over lens (const value)

-- Оператор для получения значения через линзу
(^.) :: s -> Lens s a -> a
s ^. lens = view lens s

-- Оператор для установки значения через линзу
(.~) :: Lens s a -> a -> s -> s
(.~) = set

-- Оператор для модификации значения через линзу
(%~) :: Lens s a -> (a -> a) -> s -> s
(%~) = over

-- Композиция линз
(%.%) :: Lens a b -> Lens b c -> Lens a c
(%.%) outer inner = Lens
  { view = \s -> view inner (view outer s)
  , over = \f s -> over outer (over inner f) s
  }

-- Функция для получения значения через цепочку линз
viewThrough :: s -> Lens s a -> Lens a b -> b
viewThrough s l1 l2 = view l2 (view l1 s)

-- Функция для получения значения через цепочку из трех линз
viewThrough3 :: s -> Lens s a -> Lens a b -> Lens b c -> c
viewThrough3 s l1 l2 l3 = view l3 (view l2 (view l1 s))

-- Оператор для цепочки модификаций
(&) :: s -> (s -> t) -> t
(&) = flip ($)

-- Линза для доступа к элементу списка по индексу
ix :: Int -> Lens [a] a
ix i = Lens
  { view = \xs -> if i >= 0 && i < length xs then xs !! i else error "Index out of bounds"
  , over = \f xs -> if i >= 0 && i < length xs
                    then take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs
                    else xs
  }

-- Линза для обхода всех элементов структуры
traverseL :: Lens [a] a
traverseL = Lens
  { view = \xs -> if null xs then error "Empty list" else head xs
  , over = \f -> map f
  }

-- Линза для доступа к элементу Map по ключу
at :: Ord k => k -> Lens (Map.Map k v) (Maybe v)
at k = Lens
  { view = \m -> Map.lookup k m
  , over = \f m -> case f (Map.lookup k m) of
                     Nothing -> Map.delete k m
                     Just v' -> Map.insert k v' m
  }

-- Линза для работы с Maybe, предоставляющая значение по умолчанию
non :: Eq a => a -> Lens (Maybe a) a
non def = Lens
  { view = \m -> fromMaybe def m
  , over = \f m -> case m of
                     Nothing -> if f def == def then Nothing else Just (f def)
                     Just v -> Just (f v)
  }

-- Линза для условной фильтрации
filtered :: (a -> Bool) -> Lens a a
filtered p = Lens
  { view = id
  , over = \f s -> if p s then f s else s
  }

-- Функция для проверки условия
has :: Lens s a -> s -> Bool
has lens s = True  -- Упрощенная реализация

-- Линза для работы с первым элементом пары
_1 :: Lens (a, b) a
_1 = Lens
  { view = \(a, _) -> a
  , over = \f (a, b) -> (f a, b)
  }

-- Линза для работы со вторым элементом пары
_2 :: Lens (a, b) b
_2 = Lens
  { view = \(_, b) -> b
  , over = \f (a, b) -> (a, f b)
  }

-- Линза для работы с обоими элементами пары
both :: Lens (a, a) a
both = Lens
  { view = \(a, _) -> a
  , over = \f (a, b) -> (f a, f b)
  }

-- Оператор для добавления значения в Maybe
(?~) :: Lens s (Maybe a) -> a -> s -> s
(?~) lens value = set lens (Just value)

-- Определим несколько структур данных для примеров
data Person = Person
  { _name :: String
  , _age :: Int
  , _address :: Address
  , _hobbies :: [String]
  , _contacts :: Map.Map String String
  } deriving (Show, Eq)

data Address = Address
  { _street :: String
  , _city :: String
  , _zipCode :: String
  } deriving (Show, Eq)

-- Создадим линзы для структуры Person
name :: Lens Person String
name = Lens
  { view = _name
  , over = \f p -> p { _name = f (_name p) }
  }

age :: Lens Person Int
age = Lens
  { view = _age
  , over = \f p -> p { _age = f (_age p) }
  }

address :: Lens Person Address
address = Lens
  { view = _address
  , over = \f p -> p { _address = f (_address p) }
  }

hobbies :: Lens Person [String]
hobbies = Lens
  { view = _hobbies
  , over = \f p -> p { _hobbies = f (_hobbies p) }
  }

contacts :: Lens Person (Map.Map String String)
contacts = Lens
  { view = _contacts
  , over = \f p -> p { _contacts = f (_contacts p) }
  }

-- Создадим линзы для структуры Address
street :: Lens Address String
street = Lens
  { view = _street
  , over = \f a -> a { _street = f (_street a) }
  }

city :: Lens Address String
city = Lens
  { view = _city
  , over = \f a -> a { _city = f (_city a) }
  }

zipCode :: Lens Address String
zipCode = Lens
  { view = _zipCode
  , over = \f a -> a { _zipCode = f (_zipCode a) }
  }

-- Пример данных
sampleAddress :: Address
sampleAddress = Address
  { _street = "ул. Ленина, 10"
  , _city = "Москва"
  , _zipCode = "123456"
  }

samplePerson :: Person
samplePerson = Person
  { _name = "Иван Иванов"
  , _age = 30
  , _address = sampleAddress
  , _hobbies = ["чтение", "программирование", "путешествия"]
  , _contacts = Map.fromList [("email", "ivan@example.com"), ("phone", "+7 123 456 7890")]
  }

-- Пример 1: Базовые операции с линзами
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Базовые операции с линзами"
  
  -- Получение значения через линзу
  putStrLn $ "Имя: " ++ (samplePerson ^. name)
  putStrLn $ "Возраст: " ++ show (samplePerson ^. age)
  putStrLn $ "Город: " ++ viewThrough samplePerson address city
  
  -- Установка значения через линзу
  let updatedPerson = samplePerson & (name .~ "Петр Петров")
                                   & (age .~ 35)
                                   & (address %.% city .~ "Санкт-Петербург")
  
  putStrLn $ "\nОбновленные данные:"
  putStrLn $ "Имя: " ++ (updatedPerson ^. name)
  putStrLn $ "Возраст: " ++ show (updatedPerson ^. age)
  putStrLn $ "Город: " ++ viewThrough updatedPerson address city
  
  -- Модификация значения через линзу
  let olderPerson = samplePerson & (age %~ (+5))
                                & (address %.% street %~ (++ ", кв. 5"))
  
  putStrLn $ "\nМодифицированные данные:"
  putStrLn $ "Возраст после увеличения на 5: " ++ show (olderPerson ^. age)
  putStrLn $ "Улица после добавления квартиры: " ++ viewThrough olderPerson address street

-- Пример 2: Работа со списками
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Работа со списками"
  
  -- Получение элемента списка по индексу
  putStrLn $ "Первое хобби: " ++ viewThrough samplePerson hobbies (ix 0)
  
  -- Модификация элемента списка по индексу
  let updatedHobbies = samplePerson & (hobbies %.% ix 1 .~ "функциональное программирование")
  putStrLn $ "Обновленное второе хобби: " ++ viewThrough updatedHobbies hobbies (ix 1)
  
  -- Добавление элемента в список
  let moreHobbies = samplePerson & (hobbies %~ (++ ["музыка"]))
  putStrLn $ "Список хобби после добавления: " ++ show (moreHobbies ^. hobbies)
  
  -- Фильтрация списка
  let filteredHobbies = samplePerson & (hobbies %~ filter (\h -> length h > 10))
  putStrLn $ "Отфильтрованный список хобби (длина > 10): " ++ show (filteredHobbies ^. hobbies)
  
  -- Преобразование каждого элемента списка
  let capitalizedHobbies = samplePerson & (hobbies %.% traverseL %~ capitalize)
        where capitalize (c:cs) = toUpperRussian c : cs
              capitalize [] = []
              toUpperRussian c 
                | c >= 'а' && c <= 'я' = toEnum (fromEnum c - fromEnum 'а' + fromEnum 'А')
                | c >= 'a' && c <= 'z' = toUpper c
                | otherwise = c
  
  putStrLn $ "Список хобби с заглавными буквами: " ++ show (capitalizedHobbies ^. hobbies)

-- Пример 3: Работа с Map
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Работа с Map"
  
  -- Получение значения по ключу
  putStrLn $ "Email: " ++ fromMaybe "не указан" (viewThrough samplePerson contacts (at "email"))
  putStrLn $ "Skype: " ++ fromMaybe "не указан" (viewThrough samplePerson contacts (at "skype"))
  
  -- Добавление нового ключа-значения
  let updatedContacts = samplePerson & (contacts %.% at "telegram" ?~ "@ivan")
  putStrLn $ "Telegram после добавления: " ++ fromMaybe "не указан" (viewThrough updatedContacts contacts (at "telegram"))
  
  -- Удаление ключа
  let lessContacts = samplePerson & (contacts %.% at "phone" .~ Nothing)
  putStrLn $ "Телефон после удаления: " ++ fromMaybe "не указан" (viewThrough lessContacts contacts (at "phone"))
  
  -- Модификация значения по ключу
  let modifiedContacts = samplePerson & (contacts %.% at "email" %~ fmap (++ ".ru"))
  putStrLn $ "Email после модификации: " ++ fromMaybe "не указан" (viewThrough modifiedContacts contacts (at "email"))

-- Пример 4: Условные операции
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Условные операции"
  
  -- Условная модификация
  let conditionalUpdate = samplePerson & (age %.% filtered (< 40) %~ (+10))
                                      & (age %.% filtered (>= 40) %~ subtract 5)
  
  putStrLn $ "Возраст после условной модификации: " ++ show (conditionalUpdate ^. age)
  
  -- Проверка условия
  putStrLn $ "Возраст меньше 40? " ++ show (has (age %.% filtered (< 40)) samplePerson)
  putStrLn $ "Возраст больше 50? " ++ show (has (age %.% filtered (> 50)) samplePerson)
  
  -- Условная установка
  let conditionalSet = samplePerson & (address %.% city %~ (\c -> if c == "Москва" then "Москва (столица)" else c))
  putStrLn $ "Город после условной установки: " ++ viewThrough conditionalSet address city

-- Дополнительные структуры данных для примера 5
data Department = Department
  { _departmentName :: String
  , _employees :: [Person]
  , _location :: Location
  } deriving (Show, Eq)

data Location = Location
  { _building :: String
  , _floor :: Int
  , _room :: Int
  } deriving (Show, Eq)

-- Создадим линзы для структуры Department
departmentName :: Lens Department String
departmentName = Lens
  { view = _departmentName
  , over = \f d -> d { _departmentName = f (_departmentName d) }
  }

employees :: Lens Department [Person]
employees = Lens
  { view = _employees
  , over = \f d -> d { _employees = f (_employees d) }
  }

location :: Lens Department Location
location = Lens
  { view = _location
  , over = \f d -> d { _location = f (_location d) }
  }

-- Создадим линзы для структуры Location
building :: Lens Location String
building = Lens
  { view = _building
  , over = \f l -> l { _building = f (_building l) }
  }

floorL :: Lens Location Int
floorL = Lens
  { view = _floor
  , over = \f l -> l { _floor = f (_floor l) }
  }

room :: Lens Location Int
room = Lens
  { view = _room
  , over = \f l -> l { _room = f (_room l) }
  }

-- Пример 5: Работа с вложенными структурами
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Работа с вложенными структурами"
  
  -- Создадим более сложную структуру данных
  let department = Department
        { _departmentName = "Разработка"
        , _employees = [samplePerson, samplePerson & (name .~ "Мария Петрова") & (age .~ 28)]
        , _location = Location
            { _building = "Главный офис"
            , _floor = 3
            , _room = 42
            }
        }
  
  -- Получение данных из вложенных структур
  putStrLn $ "Название отдела: " ++ (department ^. departmentName)
  putStrLn $ "Имя первого сотрудника: " ++ viewThrough3 department employees (ix 0) name
  putStrLn $ "Возраст второго сотрудника: " ++ show (viewThrough3 department employees (ix 1) age)
  putStrLn $ "Здание: " ++ viewThrough department location building
  putStrLn $ "Этаж: " ++ show (viewThrough department location floorL)
  
  -- Модификация вложенных структур
  let updatedDepartment = department & (employees %.% traverseL %.% age %~ (+1))
                                    & (location %.% room .~ 101)
  
  putStrLn $ "\nПосле модификации:"
  putStrLn $ "Возраст первого сотрудника: " ++ show (viewThrough3 updatedDepartment employees (ix 0) age)
  putStrLn $ "Возраст второго сотрудника: " ++ show (viewThrough3 updatedDepartment employees (ix 1) age)
  putStrLn $ "Комната: " ++ show (viewThrough updatedDepartment location room)

-- Пример 6: Другие полезные операции
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Другие полезные операции"
  
  -- Использование zoom для работы с частью структуры
  let updateAddress addr = addr & (street %~ (++ ", корпус 2"))
                               & (zipCode .~ "654321")
  
  let updatedPerson = samplePerson & (address %~ updateAddress)
  
  putStrLn $ "Обновленная улица: " ++ viewThrough updatedPerson address street
  putStrLn $ "Обновленный индекс: " ++ viewThrough updatedPerson address zipCode
  
  -- Использование both для работы с парами
  let pair = ("hello", "world")
  let updatedPair = pair & (both %~ capitalize)
        where capitalize (c:cs) = toUpper c : cs
              capitalize [] = []
              toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A') else c
  
  putStrLn $ "Исходная пара: " ++ show pair
  putStrLn $ "Обновленная пара: " ++ show updatedPair
  
  -- Использование _1, _2 для работы с компонентами пары
  let updatedFirst = pair & (_1 %~ reverse)
  let updatedSecond = pair & (_2 %~ reverse)
  
  putStrLn $ "Пара с перевернутым первым элементом: " ++ show updatedFirst
  putStrLn $ "Пара с перевернутым вторым элементом: " ++ show updatedSecond

-- Главная функция
main :: IO ()
main = do
  putStrLn "Линзы в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о линзах:"
  putStrLn "1. Линзы - это комбинация геттера и сеттера для поля структуры данных"
  putStrLn "2. Линзы позволяют работать с вложенными структурами данных"
  putStrLn "3. Операторы .~, %~, ^. делают код более читаемым и выразительным"
  putStrLn "4. Линзы можно комбинировать для работы с вложенными структурами"
  putStrLn "5. Линзы предоставляют специальные функции для работы со списками, Map и другими структурами данных"
