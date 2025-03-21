{-
  Композиция линз в Haskell
  
  В этом файле мы рассмотрим, как комбинировать линзы для работы с вложенными
  структурами данных и создавать сложные трансформации.
-}

module Main where

import qualified Data.Map as Map
import Data.Char (toUpper)

-- Определим несколько вложенных структур данных для примеров
data Person = Person
  { _name :: String
  , _age :: Int
  , _address :: Address
  , _contacts :: Contacts
  } deriving (Show, Eq)

data Address = Address
  { _street :: String
  , _city :: String
  , _zipCode :: String
  , _country :: String
  } deriving (Show, Eq)

data Contacts = Contacts
  { _email :: String
  , _phone :: String
  , _socialMedia :: Map.Map String String
  } deriving (Show, Eq)

data Company = Company
  { _companyName :: String
  , _employees :: [Person]
  , _departments :: Map.Map String [Person]
  , _location :: Address
  } deriving (Show, Eq)

-- Пример данных
sampleAddress :: Address
sampleAddress = Address
  { _street = "ул. Ленина, 10"
  , _city = "Москва"
  , _zipCode = "123456"
  , _country = "Россия"
  }

sampleContacts :: Contacts
sampleContacts = Contacts
  { _email = "ivan@example.com"
  , _phone = "+7 123 456 7890"
  , _socialMedia = Map.fromList
      [ ("telegram", "@ivan")
      , ("twitter", "@ivan_ivanov")
      , ("facebook", "ivan.ivanov")
      ]
  }

samplePerson :: Person
samplePerson = Person
  { _name = "Иван Иванов"
  , _age = 30
  , _address = sampleAddress
  , _contacts = sampleContacts
  }

sampleCompany :: Company
sampleCompany = Company
  { _companyName = "ООО Рога и Копыта"
  , _employees =
      [ samplePerson
      , Person "Мария Петрова" 28 sampleAddress sampleContacts
      , Person "Алексей Сидоров" 35 sampleAddress sampleContacts
      ]
  , _departments = Map.fromList
      [ ("Разработка", [samplePerson])
      , ("Маркетинг", [Person "Мария Петрова" 28 sampleAddress sampleContacts])
      , ("Продажи", [Person "Алексей Сидоров" 35 sampleAddress sampleContacts])
      ]
  , _location = sampleAddress
  }

-- Пример 1: Базовая композиция линз
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Базовая композиция линз"
  
  -- Доступ к вложенным полям через композицию линз
  putStrLn $ "Город сотрудника: " ++ _city (_address samplePerson)
  putStrLn $ "Индекс сотрудника: " ++ _zipCode (_address samplePerson)
  putStrLn $ "Email сотрудника: " ++ _email (_contacts samplePerson)
  
  -- Обновление вложенных полей через композицию линз
  let updatedPerson = samplePerson { 
        _address = (_address samplePerson) { _city = "Санкт-Петербург" },
        _contacts = (_contacts samplePerson) { _phone = "+7 987 654 3210" }
      }
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Город сотрудника: " ++ _city (_address updatedPerson)
  putStrLn $ "Телефон сотрудника: " ++ _phone (_contacts updatedPerson)
  
  -- Модификация вложенных полей через композицию линз
  let modifiedPerson = samplePerson {
        _address = (_address samplePerson) { _street = _street (_address samplePerson) ++ ", кв. 5" },
        _contacts = (_contacts samplePerson) { _email = map toUpper (_email (_contacts samplePerson)) }
      }
  
  putStrLn $ "\nПосле модификации:"
  putStrLn $ "Улица сотрудника: " ++ _street (_address modifiedPerson)
  putStrLn $ "Email сотрудника: " ++ _email (_contacts modifiedPerson)

-- Пример 2: Композиция линз для работы с коллекциями
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Композиция линз для работы с коллекциями"
  
  -- Доступ к элементам коллекций через композицию линз
  putStrLn $ "Имя первого сотрудника: " ++ _name ((_employees sampleCompany) !! 0)
  putStrLn $ "Возраст второго сотрудника: " ++ show (_age ((_employees sampleCompany) !! 1))
  putStrLn $ "Telegram первого сотрудника: " ++ 
             case Map.lookup "telegram" (_socialMedia (_contacts ((_employees sampleCompany) !! 0))) of
               Just value -> value
               Nothing -> "не указан"
  
  -- Обновление элементов коллекций через композицию линз
  let updatedCompany = sampleCompany {
        _employees = 
          let employees = _employees sampleCompany
              updatedEmployee0 = (employees !! 0) { _name = "Иван Петрович Иванов" }
              updatedEmployee1 = (employees !! 1) { _age = 29 }
              updatedEmployee0WithInstagram = updatedEmployee0 {
                _contacts = (_contacts updatedEmployee0) {
                  _socialMedia = Map.insert "instagram" "@ivan_insta" (_socialMedia (_contacts updatedEmployee0))
                }
              }
          in [updatedEmployee0WithInstagram, updatedEmployee1, employees !! 2]
      }
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя первого сотрудника: " ++ _name ((_employees updatedCompany) !! 0)
  putStrLn $ "Возраст второго сотрудника: " ++ show (_age ((_employees updatedCompany) !! 1))
  putStrLn $ "Instagram первого сотрудника: " ++ 
             case Map.lookup "instagram" (_socialMedia (_contacts ((_employees updatedCompany) !! 0))) of
               Just value -> value
               Nothing -> "не указан"

-- Пример 3: Композиция линз для работы с Map
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Композиция линз для работы с Map"
  
  -- Доступ к элементам Map через композицию линз
  putStrLn $ "Сотрудники отдела разработки: " ++ 
             show (length $ case Map.lookup "Разработка" (_departments sampleCompany) of
                     Just employees -> employees
                     Nothing -> [])
  
  putStrLn $ "Имя первого сотрудника отдела маркетинга: " ++ 
             case Map.lookup "Маркетинг" (_departments sampleCompany) of
               Just employees -> if not (null employees) then _name (employees !! 0) else "нет сотрудников"
               Nothing -> "отдел не найден"
  
  -- Обновление элементов Map через композицию линз
  let updatedCompany = sampleCompany {
        _departments = 
          let departments = _departments sampleCompany
              updatedDevelopment = case Map.lookup "Разработка" departments of
                Just employees -> if not (null employees) 
                                 then Map.insert "Разработка" 
                                      ((employees !! 0) { _name = "Иван Петрович" } : tail employees) 
                                      departments
                                 else departments
                Nothing -> departments
              updatedWithNewDepartment = Map.insert "Новый отдел" [samplePerson] updatedDevelopment
          in updatedWithNewDepartment
      }
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя первого сотрудника отдела разработки: " ++ 
             case Map.lookup "Разработка" (_departments updatedCompany) of
               Just employees -> if not (null employees) then _name (employees !! 0) else "нет сотрудников"
               Nothing -> "отдел не найден"
  
  putStrLn $ "Количество отделов: " ++ show (Map.size $ _departments updatedCompany)

-- Пример 4: Создание составных линз
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Создание составных линз"
  
  -- Создадим функции для доступа к вложенным полям
  let companyCity company = _city (_location company)
  let firstEmployeeEmail company = _email (_contacts ((_employees company) !! 0))
  
  -- Использование составных функций
  putStrLn $ "Город компании: " ++ companyCity sampleCompany
  putStrLn $ "Email первого сотрудника: " ++ firstEmployeeEmail sampleCompany
  
  -- Обновление через составные функции
  let updatedCompany = sampleCompany {
        _location = (_location sampleCompany) { _city = "Санкт-Петербург" },
        _employees = 
          let employees = _employees sampleCompany
              updatedEmployee0 = (employees !! 0) { 
                _contacts = (_contacts (employees !! 0)) { _email = "ivan.ivanov@example.com" }
              }
          in [updatedEmployee0, employees !! 1, employees !! 2]
      }
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Город компании: " ++ companyCity updatedCompany
  putStrLn $ "Email первого сотрудника: " ++ firstEmployeeEmail updatedCompany

-- Главная функция
main :: IO ()
main = do
  putStrLn "Композиция линз в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о композиции линз:"
  putStrLn "1. Линзы можно комбинировать для доступа к глубоко вложенным структурам"
  putStrLn "2. Композиция линз позволяет работать с элементами коллекций и Map"
  putStrLn "3. Можно создавать составные линзы для повторного использования"
  putStrLn "4. Композиция линз делает код более декларативным и выразительным"
