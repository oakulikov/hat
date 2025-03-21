{-
  Продвинутое использование оптики в Haskell
  
  В этом файле мы рассмотрим продвинутое использование линз, призм,
  траверсалов и других оптик для работы с вложенными структурами данных.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Lens.TH
import Control.Applicative
import Data.Monoid
import Data.Char (toUpper)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V

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
streetOfFirstEmployee :: Lens' Company String
streetOfFirstEmployee = companyEmployees . _head . personAddress . addressStreet

-- Использование zoom для работы с частью состояния
updateStreet :: String -> State Person ()
updateStreet newStreet = zoom personAddress $ do
  addressStreet .= newStreet

-- Использование at для работы с Map
type UserDB = Map.Map Int Person

lookupUser :: Int -> UserDB -> Maybe Person
lookupUser userId db = db ^. at userId

insertUser :: Int -> Person -> UserDB -> UserDB
insertUser userId person = at userId ?~ person

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

-- Использование призм с Maybe
_Just' :: Prism (Maybe a) (Maybe b) a b
_Just' = prism Just $ \case
  Just a -> Right a
  Nothing -> Left Nothing

-- Использование призм с Either
_Right' :: Prism (Either a b) (Either a c) b c
_Right' = prism Right $ \case
  Right b -> Right b
  Left a -> Left (Left a)

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

-- Изоморфизм между функциями и Kleisli стрелками
kleisliIso :: Monad m => Iso (a -> b) (c -> d) (a -> m b) (c -> m d)
kleisliIso = iso (fmap return) (>>= id)

-- Часть 6: Продвинутые геттеры и сеттеры
-- -----------------------------------

-- Создание геттера вручную
_fst :: Getter (a, b) a
_fst = to fst

-- Создание сеттера вручную
_snd :: Setter (a, b) (a, c) b c
_snd = sets (\f (a, b) -> (a, f b))

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

-- Создание фолда вручную
_sum :: Num a => Fold [a] a
_sum = folding sum

-- Использование folded для работы с Foldable
totalAge :: Fold [Person] Int
totalAge = folding (sum . map _personAge)

-- Использование filtered с folded
adultCount :: Fold [Person] Int
adultCount = folding (length . filter (\p -> _personAge p >= 18))

-- Использование foldMapOf для свертки
averageAge :: [Person] -> Double
averageAge ps = getSum totalAge / getSum count
  where
    totalAge = foldMapOf (traverse . personAge) Sum ps
    count = foldMapOf (traverse . to (const 1)) Sum ps

-- Часть 8: Комбинирование различных типов оптики
-- ------------------------------------------

-- Комбинирование линз и призм
_firstCircle :: Traversal' [Shape] Double
_firstCircle = _head . _Circle

-- Комбинирование линз и траверсалов
_allEmployeeNames :: Traversal' Company String
_allEmployeeNames = companyEmployees . traverse . personName

-- Комбинирование призм и траверсалов
_allCircleRadii :: Traversal' [Shape] Double
_allCircleRadii = traverse . _Circle

-- Комбинирование линз, призм и траверсалов
_firstEmployeeAddressIfAdult :: Traversal' Company Address
_firstEmployeeAddressIfAdult = companyEmployees . _head . filtered (\p -> p ^. personAge >= 18) . personAddress

-- Часть 9: Практические примеры использования оптики
-- ----------------------------------------------

-- Пример 1: Работа с вложенными структурами данных
data Organization = Organization
  { _orgName :: String
  , _orgDepartments :: Map.Map String Department
  } deriving (Show, Eq)

data Department = Department
  { _deptHead :: Person
  , _deptEmployees :: [Person]
  } deriving (Show, Eq)

makeLenses ''Organization
makeLenses ''Department

-- Получение имени руководителя отдела
getDeptHeadName :: String -> Organization -> Maybe String
getDeptHeadName deptName org = org ^? orgDepartments . at deptName . _Just . deptHead . personName

-- Обновление возраста всех сотрудников в организации
incrementAllOrgAges :: Organization -> Organization
incrementAllOrgAges = over (orgDepartments . traverse . deptEmployees . traverse . personAge) (+1)

-- Пример 2: Работа с JSON-подобными данными
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject (Map.Map String JsonValue)
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

_JsonObject :: Prism' JsonValue (Map.Map String JsonValue)
_JsonObject = prism JsonObject $ \case
  JsonObject o -> Right o
  other -> Left other

-- Пример данных
sampleJson :: JsonValue
sampleJson = JsonObject $ Map.fromList
  [ ("name", JsonString "John")
  , ("age", JsonNumber 30)
  , ("isAdmin", JsonBool True)
  , ("address", JsonObject $ Map.fromList
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
getJsonPath (key:keys) json = json ^? _JsonObject . at key . _Just >>= getJsonPath keys

-- Обновление значения по пути
updateJsonPath :: [String] -> (JsonValue -> JsonValue) -> JsonValue -> JsonValue
updateJsonPath [] f json = f json
updateJsonPath (key:keys) f json = json & _JsonObject . at key . _Just %~ updateJsonPath keys f

-- Пример 3: Работа с линзами в монадическом контексте
data AppState = AppState
  { _appUsers :: Map.Map Int Person
  , _appCurrentUser :: Maybe Int
  , _appSettings :: Map.Map String String
  } deriving (Show, Eq)

makeLenses ''AppState

-- Получение текущего пользователя
getCurrentUser :: AppState -> Maybe Person
getCurrentUser state = state ^? appCurrentUser . _Just >>= \userId -> state ^? appUsers . at userId . _Just

-- Обновление настроек в монадическом контексте
updateSettings :: String -> String -> State AppState ()
updateSettings key value = appSettings . at key ?= value

-- Обновление текущего пользователя в монадическом контексте
updateCurrentUser :: Int -> State AppState (Maybe Person)
updateCurrentUser userId = do
  appCurrentUser .= Just userId
  use $ appUsers . at userId

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
  
  let userDB = Map.fromList [(1, samplePerson)]
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
  
  let maybeValue = Just 42 ^? _Just'
  putStrLn $ "Значение из Just 42: " ++ show maybeValue
  
  let eitherValue = Right 42 ^? _Right'
  putStrLn $ "Значение из Right 42: " ++ show eitherValue

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Продвинутые траверсалы"
  
  let leafValues = sampleTree ^.. allLeaves
  putStrLn $ "Значения листьев: " ++ show leafValues
  
  let incrementedTree = sampleTree & allLeaves %~ (+10)
  putStrLn $ "Дерево с увеличенными значениями: " ++ show incrementedTree
  
  let sum = sumTree sampleTree
  putStrLn $ "Сумма значений в дереве: " ++ show sum
  
  let adults = sampleCompany ^.. companyEmployees . adultsOnly
  putStrLn $ "Взрослые сотрудники: " ++ show (length adults)

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Практические примеры"
  
  let org = Organization "Example Corp" $ Map.fromList
        [ ("IT", Department samplePerson [])
        , ("HR", Department (Person "HR Manager" 40 (Address "HR St" "HR City" "12345")) [])
        ]
  
  let itHeadName = getDeptHeadName "IT" org
  putStrLn $ "Имя руководителя IT отдела: " ++ show itHeadName
  
  let jsonValue = getJsonPath ["address", "city"] sampleJson
  putStrLn $ "Город из JSON: " ++ show jsonValue
  
  let updatedJson = updateJsonPath ["age"] (const (JsonNumber 31)) sampleJson
  putStrLn $ "Обновленный возраст в JSON: " ++ show (getJsonPath ["age"] updatedJson)
  
  let appState = AppState (Map.fromList [(1, samplePerson)]) (Just 1) Map.empty
  let currentUser = getCurrentUser appState
  putStrLn $ "Текущий пользователь: " ++ show currentUser

-- Главная функция
main :: IO ()
main = do
  putStrLn "Продвинутое использование оптики в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о продвинутой оптике:"
  putStrLn "1. Линзы позволяют получать и изменять поля в произведениях типов"
  putStrLn "2. Призмы позволяют работать с вариантами в суммах типов"
  putStrLn "3. Траверсалы позволяют обрабатывать несколько элементов одновременно"
  putStrLn "4. Изоморфизмы позволяют преобразовывать между изоморфными типами"
  putStrLn "5. Геттеры и сеттеры предоставляют доступ только для чтения или только для записи"
  putStrLn "6. Фолды позволяют свертывать несколько элементов"
  putStrLn "7. Различные типы оптики можно комбинировать с помощью композиции"
  putStrLn "8. Оптика особенно полезна при работе со сложными вложенными структурами данных"
