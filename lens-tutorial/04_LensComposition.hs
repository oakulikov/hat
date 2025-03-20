{-
  Композиция линз в Haskell
  
  В этом файле мы рассмотрим, как комбинировать линзы для работы с вложенными
  структурами данных и создавать сложные трансформации.
-}

module LensComposition where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
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

-- Создадим линзы с помощью Template Haskell
makeLenses ''Person
makeLenses ''Address
makeLenses ''Contacts
makeLenses ''Company

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
  putStrLn $ "Город сотрудника: " ++ (samplePerson ^. address . city)
  putStrLn $ "Индекс сотрудника: " ++ (samplePerson ^. address . zipCode)
  putStrLn $ "Email сотрудника: " ++ (samplePerson ^. contacts . email)
  
  -- Обновление вложенных полей через композицию линз
  let updatedPerson = samplePerson & address . city .~ "Санкт-Петербург"
                                  & contacts . phone .~ "+7 987 654 3210"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Город сотрудника: " ++ (updatedPerson ^. address . city)
  putStrLn $ "Телефон сотрудника: " ++ (updatedPerson ^. contacts . phone)
  
  -- Модификация вложенных полей через композицию линз
  let modifiedPerson = samplePerson & address . street %~ (++ ", кв. 5")
                                   & contacts . email %~ map toUpper
  
  putStrLn $ "\nПосле модификации:"
  putStrLn $ "Улица сотрудника: " ++ (modifiedPerson ^. address . street)
  putStrLn $ "Email сотрудника: " ++ (modifiedPerson ^. contacts . email)

-- Пример 2: Композиция линз для работы с коллекциями
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Композиция линз для работы с коллекциями"
  
  -- Доступ к элементам коллекций через композицию линз
  putStrLn $ "Имя первого сотрудника: " ++ (sampleCompany ^. employees . ix 0 . name)
  putStrLn $ "Возраст второго сотрудника: " ++ show (sampleCompany ^. employees . ix 1 . age)
  putStrLn $ "Telegram первого сотрудника: " ++ 
             (sampleCompany ^. employees . ix 0 . contacts . socialMedia . at "telegram" . non "не указан")
  
  -- Обновление элементов коллекций через композицию линз
  let updatedCompany = sampleCompany & employees . ix 0 . name .~ "Иван Петрович Иванов"
                                    & employees . ix 1 . age .~ 29
                                    & employees . ix 0 . contacts . socialMedia . at "instagram" ?~ "@ivan_insta"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя первого сотрудника: " ++ (updatedCompany ^. employees . ix 0 . name)
  putStrLn $ "Возраст второго сотрудника: " ++ show (updatedCompany ^. employees . ix 1 . age)
  putStrLn $ "Instagram первого сотрудника: " ++ 
             (updatedCompany ^. employees . ix 0 . contacts . socialMedia . at "instagram" . non "не указан")
  
  -- Модификация всех элементов коллекции через композицию линз и traversed
  let olderEmployees = sampleCompany & employees . traverse . age %~ (+1)
  
  putStrLn $ "\nПосле увеличения возраста всех сотрудников:"
  putStrLn $ "Возраст первого сотрудника: " ++ show (olderEmployees ^. employees . ix 0 . age)
  putStrLn $ "Возраст второго сотрудника: " ++ show (olderEmployees ^. employees . ix 1 . age)
  putStrLn $ "Возраст третьего сотрудника: " ++ show (olderEmployees ^. employees . ix 2 . age)

-- Пример 3: Композиция линз для работы с Map
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Композиция линз для работы с Map"
  
  -- Доступ к элементам Map через композицию линз
  putStrLn $ "Сотрудники отдела разработки: " ++ 
             show (length $ sampleCompany ^. departments . at "Разработка" . non [])
  
  putStrLn $ "Имя первого сотрудника отдела маркетинга: " ++ 
             (sampleCompany ^. departments . at "Маркетинг" . non [] . ix 0 . name)
  
  -- Обновление элементов Map через композицию линз
  let updatedCompany = sampleCompany & departments . at "Разработка" . non [] . ix 0 . name .~ "Иван Петрович"
                                    & departments . at "Новый отдел" ?~ [samplePerson]
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя первого сотрудника отдела разработки: " ++ 
             (updatedCompany ^. departments . at "Разработка" . non [] . ix 0 . name)
  
  putStrLn $ "Количество отделов: " ++ show (Map.size $ updatedCompany ^. departments)
  
  -- Модификация всех сотрудников во всех отделах
  let olderDepartments = sampleCompany & departments . traverse . traverse . age %~ (+1)
  
  putStrLn $ "\nПосле увеличения возраста всех сотрудников во всех отделах:"
  putStrLn $ "Возраст первого сотрудника отдела разработки: " ++ 
             show (olderDepartments ^. departments . at "Разработка" . non [] . ix 0 . age)
  
  putStrLn $ "Возраст первого сотрудника отдела маркетинга: " ++ 
             show (olderDepartments ^. departments . at "Маркетинг" . non [] . ix 0 . age)

-- Пример 4: Создание составных линз
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Создание составных линз"
  
  -- Создадим составную линзу для доступа к городу компании
  let companyCity = location . city
  
  -- Создадим составную линзу для доступа к email первого сотрудника
  let firstEmployeeEmail = employees . ix 0 . contacts . email
  
  -- Создадим составную линзу для доступа к телефонам всех сотрудников
  let allEmployeePhones = employees . traverse . contacts . phone
  
  -- Использование составных линз
  putStrLn $ "Город компании: " ++ (sampleCompany ^. companyCity)
  putStrLn $ "Email первого сотрудника: " ++ (sampleCompany ^. firstEmployeeEmail)
  
  -- Обновление через составные линзы
  let updatedCompany = sampleCompany & companyCity .~ "Санкт-Петербург"
                                    & firstEmployeeEmail .~ "ivan.ivanov@example.com"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Город компании: " ++ (updatedCompany ^. companyCity)
  putStrLn $ "Email первого сотрудника: " ++ (updatedCompany ^. firstEmployeeEmail)
  
  -- Модификация через составные линзы
  let modifiedCompany = sampleCompany & allEmployeePhones %~ ("+7 " ++)
  
  putStrLn $ "\nПосле модификации всех телефонов:"
  putStrLn $ "Телефон первого сотрудника: " ++ (modifiedCompany ^. employees . ix 0 . contacts . phone)
  putStrLn $ "Телефон второго сотрудника: " ++ (modifiedCompany ^. employees . ix 1 . contacts . phone)

-- Пример 5: Условная композиция линз
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Условная композиция линз"
  
  -- Создадим линзу для сотрудников старше 30 лет
  let olderThan30 = employees . traverse . filtered (\p -> p ^. age > 30)
  
  -- Создадим линзу для сотрудников из Москвы
  let fromMoscow = employees . traverse . filtered (\p -> p ^. address . city == "Москва")
  
  -- Использование условных линз
  let olderEmployees = sampleCompany ^.. olderThan30 . name
  putStrLn $ "Имена сотрудников старше 30 лет: " ++ show olderEmployees
  
  let moscowEmployees = sampleCompany ^.. fromMoscow . name
  putStrLn $ "Имена сотрудников из Москвы: " ++ show moscowEmployees
  
  -- Обновление через условные линзы
  let updatedCompany = sampleCompany & olderThan30 . contacts . phone .~ "Номер изменен"
  
  putStrLn $ "\nПосле обновления телефонов сотрудников старше 30 лет:"
  putStrLn $ "Телефон первого сотрудника (30 лет): " ++ 
             (updatedCompany ^. employees . ix 0 . contacts . phone)
  putStrLn $ "Телефон третьего сотрудника (35 лет): " ++ 
             (updatedCompany ^. employees . ix 2 . contacts . phone)

-- Пример 6: Сложные трансформации с использованием композиции линз
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Сложные трансформации с использованием композиции линз"
  
  -- Создадим функцию для форматирования адреса
  let formatAddress addr = addr ^. street ++ ", " ++ addr ^. city ++ ", " ++ addr ^. zipCode ++ ", " ++ addr ^. country
  
  -- Создадим линзу для форматированного адреса
  let formattedAddress = to formatAddress
  
  -- Создадим линзу для полного имени сотрудника с возрастом
  let fullNameWithAge = to (\p -> p ^. name ++ " (" ++ show (p ^. age) ++ " лет)")
  
  -- Использование линз с трансформациями
  putStrLn $ "Форматированный адрес компании: " ++ (sampleCompany ^. location . formattedAddress)
  
  putStrLn $ "Полные имена сотрудников с возрастом:"
  mapM_ putStrLn $ sampleCompany ^.. employees . traverse . fullNameWithAge
  
  -- Создадим линзу для получения всех email-адресов
  let allEmails = employees . traverse . contacts . email
  
  -- Создадим линзу для получения всех телефонов
  let allPhones = employees . traverse . contacts . phone
  
  -- Создадим линзу для получения всех социальных сетей
  let allSocialMedia = employees . traverse . contacts . socialMedia . traverse
  
  -- Получение всех контактных данных
  putStrLn $ "\nВсе email-адреса:"
  mapM_ putStrLn $ sampleCompany ^.. allEmails
  
  putStrLn $ "\nВсе телефоны:"
  mapM_ putStrLn $ sampleCompany ^.. allPhones
  
  putStrLn $ "\nВсе социальные сети:"
  mapM_ (\(k, v) -> putStrLn $ k ++ ": " ++ v) $ 
    sampleCompany ^.. employees . traverse . contacts . socialMedia . to Map.toList . traverse

-- Главная функция
main :: IO ()
main = do
  putStrLn "Композиция линз в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о композиции линз:"
  putStrLn "1. Линзы можно комбинировать для доступа к глубоко вложенным структурам"
  putStrLn "2. Композиция линз позволяет работать с элементами коллекций и Map"
  putStrLn "3. Можно создавать составные линзы для повторного использования"
  putStrLn "4. Условная композиция линз позволяет работать только с элементами, удовлетворяющими определенным условиям"
  putStrLn "5. Линзы можно комбинировать с трансформациями для создания сложных операций"
  putStrLn "6. Композиция линз делает код более декларативным и выразительным"
