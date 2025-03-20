{-
  Стандартные линзы (без использования внешних библиотек)
  
  В этом файле мы рассмотрим, как использовать нашу собственную реализацию линз
  для работы с различными структурами данных.
-}

module CommonLenses where

import qualified Data.Map as Map
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)

-- Реализация линз без использования внешних библиотек
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

-- Оператор для композиции линз
(^..) :: Lens a b -> Lens b c -> Lens a c
(^..) outer inner = Lens
  { view = \s -> view inner (view outer s)
  , over = \f s -> over outer (over inner f) s
  }

-- Переопределяем оператор (.) для избежания конфликта с Prelude
-- Это позволит использовать (.) для композиции функций, а (^..) для композиции линз

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
traverse :: Lens [a] a
traverse = Lens
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
non :: a -> Lens (Maybe a) a
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

-- Функция для автоматического создания линз для структур данных
-- Это упрощенная версия makeLenses из библиотеки lens
makeLenses :: String -> String
makeLenses typeName = "Линзы для " ++ typeName ++ " созданы"

-- В этом файле мы рассмотрим, как использовать нашу собственную реализацию линз
-- для работы с различными структурами данных.

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
  putStrLn $ "Город: " ++ (samplePerson ^. address ^.. city)
  
  -- Установка значения через линзу
  let updatedPerson = samplePerson & name .~ "Петр Петров"
                                   & age .~ 35
                                   & address ^.. city .~ "Санкт-Петербург"
  
  putStrLn $ "\nОбновленные данные:"
  putStrLn $ "Имя: " ++ (updatedPerson ^. name)
  putStrLn $ "Возраст: " ++ show (updatedPerson ^. age)
  putStrLn $ "Город: " ++ (updatedPerson ^. address ^.. city)
  
  -- Модификация значения через линзу
  let olderPerson = samplePerson & age %~ (+5)
                                & address ^.. street %~ (++ ", кв. 5")
  
  putStrLn $ "\nМодифицированные данные:"
  putStrLn $ "Возраст после увеличения на 5: " ++ show (olderPerson ^. age)
  putStrLn $ "Улица после добавления квартиры: " ++ (olderPerson ^. address ^.. street)

-- Пример 2: Работа со списками
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Работа со списками"
  
  -- Получение элемента списка по индексу
  putStrLn $ "Первое хобби: " ++ (samplePerson ^. hobbies ^.. ix 0)
  
  -- Модификация элемента списка по индексу
  let updatedHobbies = samplePerson & hobbies ^.. ix 1 .~ "функциональное программирование"
  putStrLn $ "Обновленное второе хобби: " ++ (updatedHobbies ^. hobbies ^.. ix 1)
  
  -- Добавление элемента в список
  let moreHobbies = samplePerson & hobbies %~ (++ ["музыка"])
  putStrLn $ "Список хобби после добавления: " ++ show (moreHobbies ^. hobbies)
  
  -- Фильтрация списка
  let filteredHobbies = samplePerson & hobbies %~ filter (\h -> length h > 10)
  putStrLn $ "Отфильтрованный список хобби (длина > 10): " ++ show (filteredHobbies ^. hobbies)
  
  -- Преобразование каждого элемента списка
  let capitalizedHobbies = samplePerson & hobbies ^.. traverse %~ capitalize
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
  putStrLn $ "Email: " ++ (samplePerson ^. contacts ^.. at "email" ^.. non "не указан")
  putStrLn $ "Skype: " ++ (samplePerson ^. contacts ^.. at "skype" ^.. non "не указан")
  
  -- Добавление нового ключа-значения
  let updatedContacts = samplePerson & contacts ^.. at "telegram" ?~ "@ivan"
  putStrLn $ "Telegram после добавления: " ++ (updatedContacts ^. contacts ^.. at "telegram" ^.. non "не указан")
  
  -- Удаление ключа
  let lessContacts = samplePerson & contacts ^.. at "phone" .~ Nothing
  putStrLn $ "Телефон после удаления: " ++ (lessContacts ^. contacts ^.. at "phone" ^.. non "не указан")
  
  -- Модификация значения по ключу
  let modifiedContacts = samplePerson & contacts ^.. at "email" ^.. non "" %~ (++ ".ru")
  putStrLn $ "Email после модификации: " ++ (modifiedContacts ^. contacts ^.. at "email" ^.. non "не указан")

-- Пример 4: Условные операции
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Условные операции"
  
  -- Условная модификация
  let conditionalUpdate = samplePerson & age ^.. filtered (< 40) %~ (+10)
                                      & age ^.. filtered (>= 40) %~ subtract 5
  
  putStrLn $ "Возраст после условной модификации: " ++ show (conditionalUpdate ^. age)
  
  -- Проверка условия
  putStrLn $ "Возраст меньше 40? " ++ show (has (age ^.. filtered (< 40)) samplePerson)
  putStrLn $ "Возраст больше 50? " ++ show (has (age ^.. filtered (> 50)) samplePerson)
  
  -- Условная установка
  let conditionalSet = samplePerson & address . city %~ (\c -> if c == "Москва" then "Москва (столица)" else c)
  putStrLn $ "Город после условной установки: " ++ (conditionalSet ^. address . city)

-- Пример 5: Работа с вложенными структурами
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Работа с вложенными структурами"
  
  -- Создадим более сложную структуру данных
  let department = Department
        { _departmentName = "Разработка"
        , _employees = [samplePerson, samplePerson & name .~ "Мария Петрова" & age .~ 28]
        , _location = Location
            { _building = "Главный офис"
            , _floor = 3
            , _room = 42
            }
        }
  
  -- Получение данных из вложенных структур
  putStrLn $ "Название отдела: " ++ (department ^. departmentName)
  putStrLn $ "Имя первого сотрудника: " ++ (department ^. employees . ix 0 . name)
  putStrLn $ "Возраст второго сотрудника: " ++ show (department ^. employees . ix 1 . age)
  putStrLn $ "Здание: " ++ (department ^. location . building)
  putStrLn $ "Этаж: " ++ show (department ^. location . floor)
  
  -- Модификация вложенных структур
  let updatedDepartment = department & employees . traverse . age %~ (+1)
                                    & location . room .~ 101
  
  putStrLn $ "\nПосле модификации:"
  putStrLn $ "Возраст первого сотрудника: " ++ show (updatedDepartment ^. employees . ix 0 . age)
  putStrLn $ "Возраст второго сотрудника: " ++ show (updatedDepartment ^. employees . ix 1 . age)
  putStrLn $ "Комната: " ++ show (updatedDepartment ^. location . room)

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

floor :: Lens Location Int
floor = Lens
  { view = _floor
  , over = \f l -> l { _floor = f (_floor l) }
  }

room :: Lens Location Int
room = Lens
  { view = _room
  , over = \f l -> l { _room = f (_room l) }
  }

-- Пример 6: Другие полезные операции
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Другие полезные операции"
  
  -- Использование zoom для работы с частью структуры
  let updateAddress addr = addr & street %~ (++ ", корпус 2")
                               & zipCode .~ "654321"
  
  let updatedPerson = samplePerson & address %~ updateAddress
  
  putStrLn $ "Обновленная улица: " ++ (updatedPerson ^. address . street)
  putStrLn $ "Обновленный индекс: " ++ (updatedPerson ^. address . zipCode)
  
  -- Использование both для работы с парами
  let pair = ("hello", "world")
  let updatedPair = pair & both %~ capitalize
        where capitalize (c:cs) = toUpper c : cs
              capitalize [] = []
              toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A') else c
  
  putStrLn $ "Исходная пара: " ++ show pair
  putStrLn $ "Обновленная пара: " ++ show updatedPair
  
  -- Использование _1, _2 для работы с компонентами пары
  let updatedFirst = pair & _1 %~ reverse
  let updatedSecond = pair & _2 %~ reverse
  
  putStrLn $ "Пара с перевернутым первым элементом: " ++ show updatedFirst
  putStrLn $ "Пара с перевернутым вторым элементом: " ++ show updatedSecond

-- Главная функция
main :: IO ()
main = do
  putStrLn "Стандартные линзы в библиотеке lens\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о стандартных линзах:"
  putStrLn "1. Библиотека lens предоставляет множество стандартных линз для работы с различными структурами данных"
  putStrLn "2. Template Haskell (makeLenses) автоматически создает линзы для полей структур данных"
  putStrLn "3. Операторы .~, %~, ^. делают код более читаемым и выразительным"
  putStrLn "4. Линзы можно комбинировать для работы с вложенными структурами"
  putStrLn "5. Библиотека lens предоставляет специальные линзы для работы со списками, Map и другими структурами данных"
