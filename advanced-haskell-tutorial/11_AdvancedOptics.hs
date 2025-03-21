{-
  Продвинутое использование оптики в Haskell
  
  В этом файле мы рассмотрим продвинутое использование линз, призм,
  траверсалов и других оптик для работы с вложенными структурами данных.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import Control.Monad.State
import Data.Char (toUpper)
import Data.List (intercalate)
-- Using association lists instead of Data.Map
type Map k v = [(k, v)]

-- Functions for Map
fromList :: [(k, v)] -> Map k v
fromList = id

empty :: Map k v
empty = []

-- Часть 1: Обзор оптики
-- ------------------

{-
Оптика - это абстракция для доступа и модификации частей структур данных.
Основные типы оптики:

1. Линзы (Lens) - для доступа и модификации полей в произведениях типов (records)
2. Призмы (Prism) - для доступа и модификации вариантов в суммах типов (ADTs)
3. Траверсалы (Traversal) - для доступа и модификации нескольких элементов
4. Изоморфизмы (Iso) - для преобразования между изоморфными типами
5. Геттеры (Getter) - для доступа к данным (только чтение)
6. Сеттеры (Setter) - для модификации данных (только запись)
7. Фолды (Fold) - для свертки нескольких элементов (только чтение)

Оптика позволяет комбинировать эти операции с помощью композиции (.).
-}

-- Часть 2: Продвинутые линзы
-- -----------------------

-- Определение типов данных
data Person = Person
  { _personName :: String
  , _personAge :: Int
  , _personAddress :: Address
  } deriving (Show, Eq)

data Address = Address
  { _addressStreet :: String
  , _addressCity :: String
  , _addressZipCode :: String
  } deriving (Show, Eq)

data Company = Company
  { _companyName :: String
  , _companyEmployees :: [Person]
  } deriving (Show, Eq)

-- Генерация линз с помощью Template Haskell
makeLenses ''Person
makeLenses ''Address
makeLenses ''Company

-- Пример данных
samplePerson :: Person
samplePerson = Person
  { _personName = "Иван Иванов"
  , _personAge = 30
  , _personAddress = Address
      { _addressStreet = "ул. Ленина, 10"
      , _addressCity = "Москва"
      , _addressZipCode = "123456"
      }
  }

sampleCompany :: Company
sampleCompany = Company
  { _companyName = "ООО Рога и Копыта"
  , _companyEmployees =
      [ samplePerson
      , Person "Мария Петрова" 28 (Address "ул. Гагарина, 5" "Москва" "123457")
      , Person "Алексей Сидоров" 35 (Address "ул. Пушкина, 15" "Санкт-Петербург" "198765")
      ]
  }

-- Продвинутые операции с линзами

-- Композиция линз
streetOfFirstEmployee :: Traversal' Company String
streetOfFirstEmployee = companyEmployees . ix 0 . personAddress . addressStreet

-- Использование zoom для работы с частью состояния
updateStreet :: String -> State Person ()
updateStreet newStreet = zoom personAddress $ do
  addressStreet .= newStreet

-- Работа с Map
type UserDB = Map Int Person

-- Простая реализация lookup для Map
lookupUser :: Int -> UserDB -> Maybe Person
lookupUser userId db = lookup userId db

-- Простая реализация insert для Map
insertUser :: Int -> Person -> UserDB -> UserDB
insertUser userId person db = (userId, person) : filter (\(k, _) -> k /= userId) db

-- Использование non для работы с Maybe
defaultAge :: Lens' (Maybe Int) Int
defaultAge = non 0

-- Часть 3: Продвинутые призмы
-- ------------------------

-- Определение типов данных
data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double
  deriving (Show, Eq)

-- Создание призм вручную
_Circle :: Prism' Shape Double
_Circle = prism Circle $ \case
  Circle r -> Right r
  other -> Left other

_Rectangle :: Prism' Shape (Double, Double)
_Rectangle = prism (uncurry Rectangle) $ \case
  Rectangle w h -> Right (w, h)
  other -> Left other

_Triangle :: Prism' Shape (Double, Double, Double)
_Triangle = prism (\(a, b, c) -> Triangle a b c) $ \case
  Triangle a b c -> Right (a, b, c)
  other -> Left other

-- Пример данных
sampleShapes :: [Shape]
sampleShapes =
  [ Circle 5.0
  , Rectangle 4.0 6.0
  , Triangle 3.0 4.0 5.0
  , Circle 2.5
  , Rectangle 2.0 3.0
  ]

-- Продвинутые операции с призмами

-- Фильтрация по типу
circles :: [Shape] -> [Double]
circles = toListOf (traverse . _Circle)

-- Модификация определенного типа
doubleCircleRadius :: Shape -> Shape
doubleCircleRadius = over _Circle (*2)

-- Использование только для проверки типа
isCircle :: Shape -> Bool
isCircle = has _Circle

-- Часть 4: Продвинутые траверсалы
-- ----------------------------

-- Определение типов данных
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Functor, Foldable, Traversable)

-- Создание траверсалов вручную
_Leaf :: Traversal' (Tree a) a
_Leaf f (Leaf a) = Leaf <$> f a
_Leaf _ (Node l r) = pure (Node l r)

-- Пример данных
sampleTree :: Tree Int
sampleTree = Node
  (Node (Leaf 1) (Leaf 2))
  (Node (Leaf 3) (Node (Leaf 4) (Leaf 5)))

-- Продвинутые операции с траверсалами

-- Обход всех листьев
allLeaves :: Traversal' (Tree a) a
allLeaves f (Leaf a) = Leaf <$> f a
allLeaves f (Node l r) = Node <$> allLeaves f l <*> allLeaves f r

-- Использование traverse для работы со списками
incrementAges :: [Person] -> [Person]
incrementAges = over (traverse . personAge) (+1)

-- Использование traversed для работы с Traversable
sumTree :: Num a => Tree a -> a
sumTree = sumOf allLeaves

-- Использование each для работы с кортежами
swapPair :: (a, a) -> (a, a)
swapPair = over each id . (\(a, b) -> (b, a))

-- Использование filtered для фильтрации
adultsOnly :: Traversal' [Person] Person
adultsOnly = traverse . filtered (\p -> p ^. personAge >= 18)

-- Часть 5: Продвинутые изоморфизмы
-- -----------------------------

-- Изоморфизм между String и [Char]
stringIso :: Iso' String [Char]
stringIso = iso id id

-- Изоморфизм между (a, b) и (b, a)
swapIso :: Iso (a, b) (c, d) (b, a) (d, c)
swapIso = iso swap swap
  where swap (a, b) = (b, a)

-- Изоморфизм между Maybe a и Either () a
maybeToEither :: Iso (Maybe a) (Maybe b) (Either () a) (Either () b)
maybeToEither = iso toEither fromEither
  where
    toEither Nothing = Left ()
    toEither (Just a) = Right a
    fromEither (Left _) = Nothing
    fromEither (Right b) = Just b

-- Часть 6: Продвинутые геттеры и сеттеры
-- -----------------------------------

-- Использование to для создания геттеров
fullName :: Getter Person String
fullName = to _personName

-- Использование mapped для работы с функторами
incrementAllAges :: Functor f => f Person -> f Person
incrementAllAges = over (mapped . personAge) (+1)

-- Использование sets для создания сеттеров
upperCaseNames :: Setter' [Person] [Person]
upperCaseNames = sets (\f ps -> f (over (traverse . personName) (map toUpper) ps))

-- Часть 7: Продвинутые фолды
-- -----------------------

-- Использование folded для работы с Foldable
totalAge :: Fold [Person] Int
totalAge = folded . personAge

-- Использование filtered с folded
adultCount :: Fold [Person] Int
adultCount = to (length . filter (\p -> p ^. personAge >= 18))

-- Использование foldMapOf для свертки
averageAge :: [Person] -> Double
averageAge ps = fromIntegral (sum ages) / fromIntegral (length ages)
  where
    ages = ps ^.. traverse . personAge

-- Часть 8: Комбинирование различных типов оптики
-- ------------------------------------------

-- Комбинирование линз и призм
_firstCircle :: Traversal' [Shape] Double
_firstCircle = ix 0 . _Circle

-- Комбинирование линз и траверсалов
_allEmployeeNames :: Traversal' Company String
_allEmployeeNames = companyEmployees . traverse . personName

-- Комбинирование призм и траверсалов
_allCircleRadii :: Traversal' [Shape] Double
_allCircleRadii = traverse . _Circle

-- Комбинирование линз, призм и траверсалов
_firstEmployeeAddressIfAdult :: Traversal' Company Address
_firstEmployeeAddressIfAdult = companyEmployees . ix 0 . filtered (\p -> p ^. personAge >= 18) . personAddress

-- Часть 9: Практические примеры использования оптики
-- ----------------------------------------------

-- Пример 1: Работа с вложенными структурами данных
data Organization = Organization
  { _orgName :: String
  , _orgDepartments :: Map String Department
  } deriving (Show, Eq)

data Department = Department
  { _deptHead :: Person
  , _deptEmployees :: [Person]
  } deriving (Show, Eq)

-- Генерация линз с помощью Template Haskell
makeLenses ''Organization
makeLenses ''Department

-- Получение имени руководителя отдела
getDeptHeadName :: String -> Organization -> Maybe String
getDeptHeadName deptName org = do
  dept <- lookup deptName (_orgDepartments org)
  return $ _personName $ _deptHead dept

-- Обновление возраста всех сотрудников в организации
incrementAllOrgAges :: Organization -> Organization
incrementAllOrgAges = over (orgDepartments . traversed . _2 . deptEmployees . traverse . personAge) (+1)

-- Пример 2: Работа с JSON-подобными данными
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (Map String JsonValue)
  deriving (Show, Eq)

-- Создание призм для JsonValue
_JsonNull :: Prism' JsonValue ()
_JsonNull = prism (const JsonNull) $ \case
  JsonNull -> Right ()
  other -> Left other

_JsonBool :: Prism' JsonValue Bool
_JsonBool = prism JsonBool $ \case
  JsonBool b -> Right b
  other -> Left other

_JsonNumber :: Prism' JsonValue Double
_JsonNumber = prism JsonNumber $ \case
  JsonNumber n -> Right n
  other -> Left other

_JsonString :: Prism' JsonValue String
_JsonString = prism JsonString $ \case
  JsonString s -> Right s
  other -> Left other

_JsonArray :: Prism' JsonValue [JsonValue]
_JsonArray = prism JsonArray $ \case
  JsonArray a -> Right a
  other -> Left other

_JsonObject :: Prism' JsonValue (Map String JsonValue)
_JsonObject = prism JsonObject $ \case
  JsonObject o -> Right o
  other -> Left other

-- Пример данных
sampleJson :: JsonValue
sampleJson = JsonObject $ fromList
  [ ("name", JsonString "John")
  , ("age", JsonNumber 30)
  , ("isAdmin", JsonBool True)
  , ("address", JsonObject $ fromList
      [ ("city", JsonString "New York")
      , ("zip", JsonNumber 10001)
      ])
  , ("hobbies", JsonArray
      [ JsonString "reading"
      , JsonString "programming"
      ])
  ]

-- Получение значения по пути
getJsonPath :: [String] -> JsonValue -> Maybe JsonValue
getJsonPath [] json = Just json
getJsonPath (key:keys) json = case json of
  JsonObject obj -> do
    value <- lookup key obj
    getJsonPath keys value
  _ -> Nothing

-- Обновление значения по пути
updateJsonPath :: [String] -> (JsonValue -> JsonValue) -> JsonValue -> JsonValue
updateJsonPath [] f json = f json
updateJsonPath (key:keys) f json = case json of
  JsonObject obj -> 
    let updateObj = map (\(k, v) -> if k == key 
                                    then (k, updateJsonPath keys f v) 
                                    else (k, v)) obj
    in JsonObject updateObj
  _ -> json

-- Пример 3: Работа с линзами в монадическом контексте
data AppState = AppState
  { _appUsers :: Map Int Person
  , _appCurrentUser :: Maybe Int
  , _appSettings :: Map String String
  } deriving (Show, Eq)

-- Генерация линз с помощью Template Haskell
makeLenses ''AppState

-- Получение текущего пользователя
getCurrentUser :: AppState -> Maybe Person
getCurrentUser state = do
  userId <- _appCurrentUser state
  lookup userId (_appUsers state)

-- Обновление настроек в монадическом контексте
updateSettings :: String -> String -> State AppState ()
updateSettings key value = do
  settings <- use appSettings
  appSettings .= ((key, value) : filter (\(k, _) -> k /= key) settings)

-- Обновление текущего пользователя в монадическом контексте
updateCurrentUser :: Int -> State AppState (Maybe Person)
updateCurrentUser userId = do
  appCurrentUser .= Just userId
  users <- use appUsers
  return $ lookup userId users

-- Часть 10: Продвинутые техники с линзами
-- -----------------------------------

-- Использование Indexed Traversal
-- Позволяет получить доступ к индексу при обходе структуры
indexedEmployees :: IndexedTraversal' Int Company Person
indexedEmployees = companyEmployees . traversed

-- Использование Indexed Fold
-- Позволяет получить доступ к индексу при свертке структуры
employeeWithIndex :: Company -> [(Int, Person)]
employeeWithIndex company = itoListOf indexedEmployees company

-- Использование Prisms с Either
eitherToMaybe :: Prism' (Either a b) b
eitherToMaybe = prism Right $ \case
  Right b -> Right b
  Left _ -> Left (Left undefined)

-- Использование Lenses с Zippers
-- Zippers позволяют перемещаться по структуре данных и модифицировать ее
-- Упрощенная реализация Zipper для дерева
data TreeZipper a = TreeZipper
  { _focus :: Tree a
  , _path :: [Either (Tree a) (Tree a)]
  } deriving (Show, Eq)

-- Функция для создания Zipper из дерева
treeZipper :: Tree a -> TreeZipper a
treeZipper t = TreeZipper t []

-- Функция для перемещения вниз по левой ветке
moveLeft :: TreeZipper a -> Maybe (TreeZipper a)
moveLeft (TreeZipper (Node l r) path) = Just $ TreeZipper l (Left r : path)
moveLeft _ = Nothing

-- Функция для перемещения вниз по правой ветке
moveRight :: TreeZipper a -> Maybe (TreeZipper a)
moveRight (TreeZipper (Node l r) path) = Just $ TreeZipper r (Right l : path)
moveRight _ = Nothing

-- Функция для перемещения вверх по дереву
moveUp :: TreeZipper a -> Maybe (TreeZipper a)
moveUp (TreeZipper t (Left r : path)) = Just $ TreeZipper (Node t r) path
moveUp (TreeZipper t (Right l : path)) = Just $ TreeZipper (Node l t) path
moveUp _ = Nothing

-- Функция для модификации текущего узла
modifyFocus :: (Tree a -> Tree a) -> TreeZipper a -> TreeZipper a
modifyFocus f (TreeZipper t path) = TreeZipper (f t) path

-- Функция для получения дерева из Zipper
fromZipper :: TreeZipper a -> Tree a
fromZipper (TreeZipper t []) = t
fromZipper z = case moveUp z of
  Just z' -> fromZipper z'
  Nothing -> _focus z

-- Часть 11: Продвинутые комбинаторы линз
-- ----------------------------------

-- Использование alongside для работы с двумя структурами одновременно
combineTwoPersons :: (Person, Person) -> (String, String)
combineTwoPersons = view (alongside personName personName)

-- Использование choosing для выбора между двумя структурами
chooseNameOrCity :: Either Person Address -> String
chooseNameOrCity = view (choosing personName addressCity)

-- Использование map для работы с контейнерами
mapPersonNames :: [Person] -> [String]
mapPersonNames = map (view personName)

-- Использование singular для работы с одним элементом
firstPersonName :: [Person] -> Maybe String
firstPersonName = preview (singular traverse . personName)

-- Часть 12: Линзы и типы-функции
-- -------------------------

-- Использование Lens с типами-функциями
type Predicate a = a -> Bool

-- Линза для доступа к предикату
predicate :: Lens' (Predicate a) (a -> Bool)
predicate = lens id const

-- Модификация предиката
negatePredicate :: Predicate a -> Predicate a
negatePredicate = over predicate (\f -> not . f)

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Продвинутые линзы"
  
  putStrLn $ "Имя: " ++ (samplePerson ^. personName)
  putStrLn $ "Возраст: " ++ show (samplePerson ^. personAge)
  putStrLn $ "Город: " ++ (samplePerson ^. personAddress . addressCity)
  
  let updatedPerson = samplePerson & personName .~ "Петр Петров"
                                   & personAge +~ 5
                                   & personAddress . addressCity .~ "Санкт-Петербург"
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Имя: " ++ (updatedPerson ^. personName)
  putStrLn $ "Возраст: " ++ show (updatedPerson ^. personAge)
  putStrLn $ "Город: " ++ (updatedPerson ^. personAddress . addressCity)
  
  let street = sampleCompany ^. streetOfFirstEmployee
  putStrLn $ "\nУлица первого сотрудника: " ++ street
  
  let userDB = fromList [(1, samplePerson)]
  let user = lookupUser 1 userDB
  putStrLn $ "\nПользователь с ID 1: " ++ show user
  
  let age = Nothing ^. defaultAge
  putStrLn $ "\nВозраст по умолчанию: " ++ show age

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Продвинутые призмы"
  
  let circleRadii = circles sampleShapes
  putStrLn $ "Радиусы кругов: " ++ show circleRadii
  
  let doubledCircles = map doubleCircleRadius sampleShapes
  putStrLn $ "Круги с удвоенным радиусом: " ++ show doubledCircles
  
  let shape = Circle 5.0
  putStrLn $ "Это круг? " ++ show (isCircle shape)
  
  let maybeValue = Just 42 ^? _Just
  putStrLn $ "Значение из Just 42: " ++ show maybeValue
  
  let eitherValue = Right 42 ^? _Right
  putStrLn $ "Значение из Right 42: " ++ show eitherValue

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Продвинутые траверсалы"
  
  let leafValues = toListOf allLeaves sampleTree
  putStrLn $ "Значения листьев: " ++ show leafValues
  
  let incrementedTree = over allLeaves (+10) sampleTree
  putStrLn $ "Дерево с увеличенными значениями: " ++ show incrementedTree
  
  let sum = sumTree sampleTree
  putStrLn $ "Сумма значений в дереве: " ++ show sum
  
  let adults = sampleCompany ^.. companyEmployees . adultsOnly
  putStrLn $ "Взрослые сотрудники: " ++ show (length adults)

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Практические примеры"
  
  let org = Organization "Example Corp" $ fromList
        [ ("IT", Department samplePerson [])
        , ("HR", Department (Person "HR Manager" 40 (Address "HR St" "HR City" "12345")) [])
        ]
  
  let itHeadName = getDeptHeadName "IT" org
  putStrLn $ "Имя руководителя IT отдела: " ++ show itHeadName
  
  let jsonValue = getJsonPath ["address", "city"] sampleJson
  putStrLn $ "Город из JSON: " ++ show jsonValue
  
  let updatedJson = updateJsonPath ["age"] (const (JsonNumber 31)) sampleJson
  putStrLn $ "Обновленный возраст в JSON: " ++ show (getJsonPath ["age"] updatedJson)
  
  let appState = AppState (fromList [(1, samplePerson)]) (Just 1) empty
  let currentUser = getCurrentUser appState
  putStrLn $ "Текущий пользователь: " ++ show currentUser

example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Продвинутые техники с линзами"
  
  let employeesWithIndex = employeeWithIndex sampleCompany
  putStrLn $ "Сотрудники с индексами: " ++ show (map fst employeesWithIndex)
  
  let isEven = negatePredicate odd
  putStrLn $ "Проверка на четность числа 4: " ++ show (isEven 4)
  putStrLn $ "Проверка на четность числа 5: " ++ show (isEven 5)
  
  let twoPersons = (samplePerson, samplePerson & personName .~ "Петр")
  let names = combineTwoPersons twoPersons
  putStrLn $ "Имена двух сотрудников: " ++ show names
  
  let firstPerson = firstPersonName (sampleCompany ^. companyEmployees)
  putStrLn $ "Имя первого сотрудника: " ++ show firstPerson

-- Главная функция
main :: IO ()
main = do
  putStrLn "Продвинутое использование оптики в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  
  putStrLn "\nКлючевые моменты о продвинутой оптике:"
  putStrLn "1. Линзы позволяют получать и изменять поля в произведениях типов"
  putStrLn "2. Призмы позволяют работать с вариантами в суммах типов"
  putStrLn "3. Траверсалы позволяют обрабатывать несколько элементов одновременно"
  putStrLn "4. Изоморфизмы позволяют преобразовывать между изоморфными типами"
  putStrLn "5. Геттеры и сеттеры предоставляют доступ только для чтения или только для записи"
  putStrLn "6. Фолды позволяют свертывать несколько элементов"
  putStrLn "7. Различные типы оптики можно комбинировать с помощью композиции"
  putStrLn "8. Оптика особенно полезна при работе со сложными вложенными структурами данных"
  putStrLn "9. Продвинутые техники, такие как Indexed Traversal и Zippers, расширяют возможности оптики"
  putStrLn "10. Линзы могут работать с типами-функциями и другими сложными структурами данных"
