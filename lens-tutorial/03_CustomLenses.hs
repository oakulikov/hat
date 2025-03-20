{-
  Создание собственных линз в Haskell
  
  В этом файле мы рассмотрим, как создавать собственные линзы без использования
  Template Haskell, а также как создавать линзы для более сложных случаев.
-}

module CustomLenses where

-- Импортируем только базовые типы из библиотеки lens
import Control.Lens.Getter (Getter, to, (^.))
import Control.Lens.Setter (Setter, set, over, (.~), (%~))
import Control.Lens.Lens (Lens, lens)
import Control.Lens.Prism (Prism, prism)
import Control.Lens.Traversal (Traversal, traversed)
import Control.Lens.Iso (Iso, iso)
import Data.Char (toUpper, toLower)
import Data.List (intercalate)

-- Простейшая реализация линзы
-- Линза - это комбинация геттера и сеттера для поля структуры данных
data SimpleLens s a = SimpleLens
  { view :: s -> a           -- Функция для получения значения
  , set :: a -> s -> s       -- Функция для установки значения
  }

-- Пример 1: Создание линз вручную
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Создание линз вручную"
  
  -- Определим структуру данных
  let person = Person "Иван Иванов" 30
  
  -- Создадим линзы вручную
  let nameLens = SimpleLens
        { view = name
        , set = \newName p -> p { name = newName }
        }
  
  let ageLens = SimpleLens
        { view = age
        , set = \newAge p -> p { age = newAge }
        }
  
  -- Использование линз
  putStrLn $ "Имя: " ++ view nameLens person
  putStrLn $ "Возраст: " ++ show (view ageLens person)
  
  -- Обновление данных через линзы
  let updatedPerson = set nameLens "Петр Петров" person
  let olderPerson = set ageLens 35 updatedPerson
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя: " ++ view nameLens olderPerson
  putStrLn $ "Возраст: " ++ show (view ageLens olderPerson)
  
  -- Создание функции для модификации значения через линзу
  let over lens f s = set lens (f (view lens s)) s
  
  -- Модификация данных через линзы
  let evenOlderPerson = over ageLens (+5) olderPerson
  
  putStrLn $ "\nПосле модификации:"
  putStrLn $ "Возраст после увеличения на 5: " ++ show (view ageLens evenOlderPerson)

-- Структура данных для примера 1
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Eq)

-- Пример 2: Создание линз с использованием функции lens из библиотеки lens
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Создание линз с использованием функции lens"
  
  -- Определим структуру данных
  let address = Address "ул. Ленина, 10" "Москва" "123456"
  
  -- Создадим линзы с использованием функции lens
  let streetLens = lens street (\a s -> a { street = s })
  let cityLens = lens city (\a s -> a { city = s })
  let zipCodeLens = lens zipCode (\a s -> a { zipCode = s })
  
  -- Использование линз
  putStrLn $ "Улица: " ++ (address ^. streetLens)
  putStrLn $ "Город: " ++ (address ^. cityLens)
  putStrLn $ "Индекс: " ++ (address ^. zipCodeLens)
  
  -- Обновление данных через линзы
  let updatedAddress = address & streetLens .~ "ул. Пушкина, 15"
                              & cityLens .~ "Санкт-Петербург"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Улица: " ++ (updatedAddress ^. streetLens)
  putStrLn $ "Город: " ++ (updatedAddress ^. cityLens)
  
  -- Модификация данных через линзы
  let modifiedAddress = address & streetLens %~ (++ ", кв. 5")
                               & zipCodeLens %~ reverse
  
  putStrLn $ "\nПосле модификации:"
  putStrLn $ "Улица после добавления квартиры: " ++ (modifiedAddress ^. streetLens)
  putStrLn $ "Индекс после переворота: " ++ (modifiedAddress ^. zipCodeLens)

-- Структура данных для примера 2
data Address = Address
  { street :: String
  , city :: String
  , zipCode :: String
  } deriving (Show, Eq)

-- Пример 3: Создание линз для вложенных структур
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Создание линз для вложенных структур"
  
  -- Определим вложенные структуры данных
  let employee = Employee "Иван Иванов" 30 (Address "ул. Ленина, 10" "Москва" "123456")
  
  -- Создадим линзы для Employee
  let employeeNameLens = lens employeeName (\e n -> e { employeeName = n })
  let employeeAgeLens = lens employeeAge (\e a -> e { employeeAge = a })
  let employeeAddressLens = lens employeeAddress (\e a -> e { employeeAddress = a })
  
  -- Создадим линзы для Address
  let streetLens = lens street (\a s -> a { street = s })
  let cityLens = lens city (\a s -> a { city = s })
  let zipCodeLens = lens zipCode (\a s -> a { zipCode = s })
  
  -- Создадим составные линзы для доступа к полям адреса через сотрудника
  let employeeStreetLens = employeeAddressLens . streetLens
  let employeeCityLens = employeeAddressLens . cityLens
  let employeeZipCodeLens = employeeAddressLens . zipCodeLens
  
  -- Использование составных линз
  putStrLn $ "Имя сотрудника: " ++ (employee ^. employeeNameLens)
  putStrLn $ "Улица сотрудника: " ++ (employee ^. employeeStreetLens)
  putStrLn $ "Город сотрудника: " ++ (employee ^. employeeCityLens)
  
  -- Обновление вложенных данных через составные линзы
  let updatedEmployee = employee & employeeNameLens .~ "Петр Петров"
                                & employeeCityLens .~ "Санкт-Петербург"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя сотрудника: " ++ (updatedEmployee ^. employeeNameLens)
  putStrLn $ "Город сотрудника: " ++ (updatedEmployee ^. employeeCityLens)

-- Структура данных для примера 3
data Employee = Employee
  { employeeName :: String
  , employeeAge :: Int
  , employeeAddress :: Address
  } deriving (Show, Eq)

-- Пример 4: Создание линз для виртуальных полей
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Создание линз для виртуальных полей"
  
  -- Определим структуру данных
  let person = FullName "Иван" "Иванович" "Иванов"
  
  -- Создадим линзы для отдельных полей
  let firstNameLens = lens firstName (\p n -> p { firstName = n })
  let middleNameLens = lens middleName (\p n -> p { middleName = n })
  let lastNameLens = lens lastName (\p n -> p { lastName = n })
  
  -- Создадим линзу для виртуального поля - полного имени
  let fullNameLens = lens
        -- Геттер: объединяет отдельные части имени
        (\p -> firstName p ++ " " ++ middleName p ++ " " ++ lastName p)
        -- Сеттер: разбивает полное имя на части
        (\p fullName ->
          let parts = words fullName
              fn = if length parts > 0 then parts !! 0 else ""
              mn = if length parts > 1 then parts !! 1 else ""
              ln = if length parts > 2 then parts !! 2 else ""
          in p { firstName = fn, middleName = mn, lastName = ln }
        )
  
  -- Создадим линзу для виртуального поля - инициалов
  let initialsLens = lens
        -- Геттер: формирует инициалы из первых букв имени, отчества и фамилии
        (\p -> [head (firstName p), head (middleName p), head (lastName p)])
        -- Сеттер: не имеет смысла для инициалов, поэтому просто возвращаем исходный объект
        (\p _ -> p)
  
  -- Использование линз для виртуальных полей
  putStrLn $ "Полное имя: " ++ (person ^. fullNameLens)
  putStrLn $ "Инициалы: " ++ (person ^. initialsLens)
  
  -- Обновление через линзу для виртуального поля
  let updatedPerson = person & fullNameLens .~ "Петр Петрович Петров"
  
  putStrLn $ "\nПосле обновления через fullNameLens:"
  putStrLn $ "Имя: " ++ (updatedPerson ^. firstNameLens)
  putStrLn $ "Отчество: " ++ (updatedPerson ^. middleNameLens)
  putStrLn $ "Фамилия: " ++ (updatedPerson ^. lastNameLens)
  putStrLn $ "Полное имя: " ++ (updatedPerson ^. fullNameLens)
  putStrLn $ "Инициалы: " ++ (updatedPerson ^. initialsLens)

-- Структура данных для примера 4
data FullName = FullName
  { firstName :: String
  , middleName :: String
  , lastName :: String
  } deriving (Show, Eq)

-- Пример 5: Создание линз для сложных преобразований
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Создание линз для сложных преобразований"
  
  -- Определим структуру данных
  let user = User "ivan_ivanov" "password123" "ivan@example.com"
  
  -- Создадим линзы для отдельных полей
  let usernameLens = lens username (\u n -> u { username = n })
  let passwordLens = lens password (\u p -> u { password = p })
  let emailLens = lens email (\u e -> u { email = e })
  
  -- Создадим линзу для преобразования имени пользователя в верхний регистр
  let uppercaseUsernameLens = lens
        -- Геттер: преобразует имя пользователя в верхний регистр
        (\u -> map toUpper (username u))
        -- Сеттер: устанавливает имя пользователя, преобразуя его в нижний регистр
        (\u n -> u { username = map toLower n })
  
  -- Создадим линзу для домена электронной почты
  let emailDomainLens = lens
        -- Геттер: извлекает домен из адреса электронной почты
        (\u -> let e = email u
                   parts = break (== '@') e
               in if snd parts /= "" then tail (snd parts) else "")
        -- Сеттер: обновляет домен в адресе электронной почты
        (\u d -> let e = email u
                     parts = break (== '@') e
                 in if snd parts /= ""
                    then u { email = fst parts ++ "@" ++ d }
                    else u { email = e ++ "@" ++ d })
  
  -- Использование линз для сложных преобразований
  putStrLn $ "Имя пользователя: " ++ (user ^. usernameLens)
  putStrLn $ "Имя пользователя в верхнем регистре: " ++ (user ^. uppercaseUsernameLens)
  putStrLn $ "Домен электронной почты: " ++ (user ^. emailDomainLens)
  
  -- Обновление через линзы для сложных преобразований
  let updatedUser = user & uppercaseUsernameLens .~ "PETER_PETROV"
                         & emailDomainLens .~ "gmail.com"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя пользователя: " ++ (updatedUser ^. usernameLens)
  putStrLn $ "Электронная почта: " ++ (updatedUser ^. emailLens)

-- Структура данных для примера 5
data User = User
  { username :: String
  , password :: String
  , email :: String
  } deriving (Show, Eq)

-- Пример 6: Создание линз для полиморфных структур данных
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Создание линз для полиморфных структур данных"
  
  -- Определим полиморфную структуру данных
  let intPair = Pair 10 20 :: Pair Int Int
  let stringPair = Pair "hello" "world" :: Pair String String
  let mixedPair = Pair 42 "answer" :: Pair Int String
  
  -- Создадим полиморфные линзы для Pair
  let firstLens = lens first (\p x -> p { first = x })
  let secondLens = lens second (\p y -> p { second = y })
  
  -- Использование полиморфных линз
  putStrLn $ "Первый элемент intPair: " ++ show (intPair ^. firstLens)
  putStrLn $ "Второй элемент stringPair: " ++ (stringPair ^. secondLens)
  putStrLn $ "Первый элемент mixedPair: " ++ show (mixedPair ^. firstLens)
  putStrLn $ "Второй элемент mixedPair: " ++ (mixedPair ^. secondLens)
  
  -- Обновление через полиморфные линзы
  let updatedIntPair = intPair & firstLens .~ 100
                              & secondLens .~ 200
  
  let updatedStringPair = stringPair & firstLens .~ "привет"
                                    & secondLens .~ "мир"
  
  let updatedMixedPair = mixedPair & firstLens .~ 99
                                  & secondLens .~ "ninety-nine"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Обновленный intPair: " ++ show (updatedIntPair ^. firstLens) ++ ", " ++ show (updatedIntPair ^. secondLens)
  putStrLn $ "Обновленный stringPair: " ++ (updatedStringPair ^. firstLens) ++ ", " ++ (updatedStringPair ^. secondLens)
  putStrLn $ "Обновленный mixedPair: " ++ show (updatedMixedPair ^. firstLens) ++ ", " ++ (updatedMixedPair ^. secondLens)

-- Полиморфная структура данных для примера 6
data Pair a b = Pair
  { first :: a
  , second :: b
  } deriving (Show, Eq)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Создание собственных линз в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о создании собственных линз:"
  putStrLn "1. Линзы можно создавать вручную, определяя функции для получения и установки значений"
  putStrLn "2. Функция lens из библиотеки lens упрощает создание линз"
  putStrLn "3. Линзы можно комбинировать для доступа к вложенным структурам"
  putStrLn "4. Линзы можно создавать для виртуальных полей, которые не существуют в исходной структуре данных"
  putStrLn "5. Линзы могут выполнять сложные преобразования данных"
  putStrLn "6. Линзы могут работать с полиморфными структурами данных"
