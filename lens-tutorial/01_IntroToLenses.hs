{-
  Введение в линзы в Haskell
  
  Линзы - это абстракция, которая позволяет работать с вложенными структурами данных
  в функциональном стиле. Они решают проблему обновления вложенных полей в
  неизменяемых структурах данных.
-}

module IntroToLenses where

-- Проблема: обновление вложенных полей в неизменяемых структурах данных

-- Определим несколько вложенных структур данных
data Address = Address
  { street :: String
  , city :: String
  , zipCode :: String
  } deriving (Show, Eq)

data Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  } deriving (Show, Eq)

data Company = Company
  { companyName :: String
  , employees :: [Person]
  } deriving (Show, Eq)

-- Пример данных
sampleAddress :: Address
sampleAddress = Address
  { street = "ул. Ленина, 10"
  , city = "Москва"
  , zipCode = "123456"
  }

samplePerson :: Person
samplePerson = Person
  { name = "Иван Иванов"
  , age = 30
  , address = sampleAddress
  }

sampleCompany :: Company
sampleCompany = Company
  { companyName = "ООО Рога и Копыта"
  , employees = [samplePerson]
  }

-- Проблема 1: Обновление вложенных полей
-- Допустим, мы хотим изменить город в адресе сотрудника компании
-- Традиционный подход:

updateEmployeeCity :: Company -> String -> Company
updateEmployeeCity company newCity =
  company { employees = updatedEmployees }
  where
    updatedEmployees = map updatePersonCity (employees company)
    updatePersonCity person =
      person { address = updateAddressCity (address person) }
    updateAddressCity addr =
      addr { city = newCity }

-- Это работает, но код становится громоздким при увеличении вложенности

-- Проблема 2: Доступ к вложенным полям
-- Допустим, мы хотим получить город из адреса сотрудника компании
getEmployeeCity :: Company -> Maybe String
getEmployeeCity company =
  case employees company of
    [] -> Nothing
    (firstEmployee:_) -> Just $ city $ address firstEmployee

-- Опять же, код становится громоздким при увеличении вложенности

-- Решение: Линзы
-- Линза - это комбинация геттера и сеттера для поля структуры данных

-- Простейшая реализация линзы
data Lens s a = Lens
  { getter :: s -> a           -- Функция для получения значения
  , setter :: a -> s -> s      -- Функция для установки значения
  }

-- Создадим линзы для наших структур данных

-- Линза для поля name в Person
nameLens :: Lens Person String
nameLens = Lens
  { getter = name
  , setter = \newName person -> person { name = newName }
  }

-- Линза для поля age в Person
ageLens :: Lens Person Int
ageLens = Lens
  { getter = age
  , setter = \newAge person -> person { age = newAge }
  }

-- Линза для поля address в Person
addressLens :: Lens Person Address
addressLens = Lens
  { getter = address
  , setter = \newAddress person -> person { address = newAddress }
  }

-- Линза для поля street в Address
streetLens :: Lens Address String
streetLens = Lens
  { getter = street
  , setter = \newStreet addr -> addr { street = newStreet }
  }

-- Линза для поля city в Address
cityLens :: Lens Address String
cityLens = Lens
  { getter = city
  , setter = \newCity addr -> addr { city = newCity }
  }

-- Линза для поля zipCode в Address
zipCodeLens :: Lens Address String
zipCodeLens = Lens
  { getter = zipCode
  , setter = \newZipCode addr -> addr { zipCode = newZipCode }
  }

-- Линза для поля companyName в Company
companyNameLens :: Lens Company String
companyNameLens = Lens
  { getter = companyName
  , setter = \newName company -> company { companyName = newName }
  }

-- Линза для поля employees в Company
employeesLens :: Lens Company [Person]
employeesLens = Lens
  { getter = employees
  , setter = \newEmployees company -> company { employees = newEmployees }
  }

-- Функции для работы с линзами

-- Получение значения через линзу
view :: Lens s a -> s -> a
view lens = getter lens

-- Установка значения через линзу
set :: Lens s a -> a -> s -> s
set lens = setter lens

-- Модификация значения через линзу
over :: Lens s a -> (a -> a) -> s -> s
over lens f s = set lens (f (view lens s)) s

-- Композиция линз
-- Это ключевая особенность линз - возможность их композиции
compose :: Lens s a -> Lens a b -> Lens s b
compose outer inner = Lens
  { getter = \s -> getter inner (getter outer s)
  , setter = \b s -> setter outer (setter inner b (getter outer s)) s
  }

-- Оператор для композиции линз
(^.) :: Lens s a -> Lens a b -> Lens s b
(^.) = compose

-- Теперь мы можем использовать линзы для работы с вложенными структурами

-- Пример 1: Получение значения
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Получение значения через линзу"
  
  let personName = view nameLens samplePerson
  putStrLn $ "Имя: " ++ personName
  
  let personCity = view (addressLens ^. cityLens) samplePerson
  putStrLn $ "Город: " ++ personCity

-- Пример 2: Установка значения
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Установка значения через линзу"
  
  let updatedPerson = set nameLens "Петр Петров" samplePerson
  putStrLn $ "Обновленное имя: " ++ name updatedPerson
  
  let updatedPerson2 = set (addressLens ^. cityLens) "Санкт-Петербург" samplePerson
  putStrLn $ "Обновленный город: " ++ (city $ address updatedPerson2)

-- Пример 3: Модификация значения
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Модификация значения через линзу"
  
  let updatedPerson = over ageLens (+1) samplePerson
  putStrLn $ "Возраст после увеличения на 1: " ++ show (age updatedPerson)
  
  let updatedPerson2 = over (addressLens ^. streetLens) (++ ", кв. 5") samplePerson
  putStrLn $ "Обновленная улица: " ++ (street $ address updatedPerson2)

-- Пример 4: Композиция линз для глубоко вложенных структур
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Композиция линз для глубоко вложенных структур"
  
  -- Создадим линзу для доступа к городу первого сотрудника компании
  -- Для простоты мы не обрабатываем случай пустого списка сотрудников
  let firstEmployeeLens = Lens
        { getter = \company -> head (employees company)
        , setter = \newEmployee company -> 
            company { employees = newEmployee : tail (employees company) }
        }
  
  -- Это неправильная композиция, так как companyNameLens возвращает String, а не Company
  -- Правильная композиция будет такой:
  let companyEmployeeCityLens = firstEmployeeLens ^. addressLens ^. cityLens
  
  -- Получение города первого сотрудника
  let employeeCity = view (firstEmployeeLens ^. addressLens ^. cityLens) sampleCompany
  putStrLn $ "Город первого сотрудника: " ++ employeeCity
  
  -- Обновление города первого сотрудника
  let updatedCompany = set (firstEmployeeLens ^. addressLens ^. cityLens) "Казань" sampleCompany
  let updatedCity = city $ address $ head $ employees updatedCompany
  putStrLn $ "Обновленный город первого сотрудника: " ++ updatedCity

-- Пример 5: Преимущества линз
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Преимущества линз"
  
  putStrLn "1. Композиция - линзы можно комбинировать для доступа к глубоко вложенным структурам"
  putStrLn "2. Единый интерфейс - одинаковый способ работы с разными структурами данных"
  putStrLn "3. Декларативность - код с линзами более декларативный и выразительный"
  putStrLn "4. Переиспользуемость - линзы можно переиспользовать в разных частях кода"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Введение в линзы в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  
  putStrLn "\nКлючевые моменты о линзах:"
  putStrLn "1. Линза - это комбинация геттера и сеттера для поля структуры данных"
  putStrLn "2. Линзы позволяют работать с вложенными структурами данных в функциональном стиле"
  putStrLn "3. Линзы можно композировать для доступа к глубоко вложенным полям"
  putStrLn "4. Линзы делают код более декларативным и поддерживаемым"
  putStrLn "5. В реальных проектах обычно используется библиотека lens, которая предоставляет более мощные возможности"
