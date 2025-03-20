{-
  Призмы и траверсалы в Haskell
  
  В этом файле мы рассмотрим другие оптики, связанные с линзами:
  призмы (Prism) и траверсалы (Traversal).
-}

module PrismsAndTraversals where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char (toUpper, toLower, isDigit)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, isJust, fromJust)

-- Часть 1: Призмы (Prism)
-- Призмы - это оптики, которые фокусируются на одном конструкторе
-- алгебраического типа данных. Они позволяют "заглянуть" внутрь
-- значения и извлечь данные, если они соответствуют определенному
-- конструктору, или построить значение из данных.

-- Пример 1: Основы призм
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Основы призм"
  
  -- Определим алгебраический тип данных
  let success = Success "Operation completed" 200
  let failure = Failure "Not found" 404
  let pending = Pending "Processing"
  
  -- Создадим призмы для каждого конструктора
  let _Success = prism 
        (\(msg, code) -> Success msg code)  -- Конструктор
        (\r -> case r of                    -- Деконструктор
                Success msg code -> Right (msg, code)
                other -> Left other)
  
  let _Failure = prism 
        (\(msg, code) -> Failure msg code)
        (\r -> case r of
                Failure msg code -> Right (msg, code)
                other -> Left other)
  
  let _Pending = prism 
        Pending
        (\r -> case r of
                Pending msg -> Right msg
                other -> Left other)
  
  -- Использование призм для проверки типа
  putStrLn $ "success это Success? " ++ show (is _Success success)
  putStrLn $ "failure это Success? " ++ show (is _Success failure)
  putStrLn $ "pending это Pending? " ++ show (is _Pending pending)
  
  -- Использование призм для извлечения данных
  putStrLn $ "\nИзвлечение данных из success:"
  case preview _Success success of
    Just (msg, code) -> putStrLn $ "  Сообщение: " ++ msg ++ ", Код: " ++ show code
    Nothing -> putStrLn "  Не Success"
  
  putStrLn $ "\nИзвлечение данных из failure:"
  case preview _Failure failure of
    Just (msg, code) -> putStrLn $ "  Сообщение: " ++ msg ++ ", Код: " ++ show code
    Nothing -> putStrLn "  Не Failure"
  
  -- Использование призм для обновления данных
  let updatedSuccess = success & _Success . _1 .~ "Operation successfully completed"
  let updatedFailure = failure & _Failure . _2 .~ 500
  
  putStrLn $ "\nПосле обновления:"
  case preview _Success updatedSuccess of
    Just (msg, code) -> putStrLn $ "  Success: " ++ msg ++ ", Код: " ++ show code
    Nothing -> putStrLn "  Не Success"
  
  case preview _Failure updatedFailure of
    Just (msg, code) -> putStrLn $ "  Failure: " ++ msg ++ ", Код: " ++ show code
    Nothing -> putStrLn "  Не Failure"
  
  -- Создание новых значений через призмы
  let newSuccess = review _Success ("Created via prism", 201)
  let newPending = review _Pending "Waiting for approval"
  
  putStrLn $ "\nСозданные через призмы значения:"
  putStrLn $ "  newSuccess: " ++ show newSuccess
  putStrLn $ "  newPending: " ++ show newPending

-- Тип данных для примера 1
data Result = Success String Int
            | Failure String Int
            | Pending String
            deriving (Show, Eq)

-- Пример 2: Призмы для стандартных типов данных
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Призмы для стандартных типов данных"
  
  -- Призма для Maybe
  let _Just = prism 
        id  -- Конструктор: просто возвращаем значение
        (\m -> case m of
                Just a -> Right a
                Nothing -> Left Nothing)
  
  let _Nothing = prism' 
        (\() -> Nothing)  -- Конструктор: создаем Nothing из ()
        (\m -> case m of
                Nothing -> Just ()
                _ -> Nothing)
  
  -- Призма для Either
  let _Left = prism 
        Left
        (\e -> case e of
                Left a -> Right a
                Right b -> Left (Right b))
  
  let _Right = prism 
        Right
        (\e -> case e of
                Right b -> Right b
                Left a -> Left (Left a))
  
  -- Использование призм для Maybe
  let justValue = Just 42
  let nothingValue = Nothing :: Maybe Int
  
  putStrLn $ "justValue это Just? " ++ show (is _Just justValue)
  putStrLn $ "nothingValue это Nothing? " ++ show (is _Nothing nothingValue)
  
  putStrLn $ "\nИзвлечение значения из justValue: " ++ 
             show (fromMaybe 0 (preview _Just justValue))
  
  let updatedJust = justValue & _Just %~ (*2)
  putStrLn $ "justValue после умножения на 2: " ++ show updatedJust
  
  -- Использование призм для Either
  let leftValue = Left "error" :: Either String Int
  let rightValue = Right 42 :: Either String Int
  
  putStrLn $ "\nleftValue это Left? " ++ show (is _Left leftValue)
  putStrLn $ "rightValue это Right? " ++ show (is _Right rightValue)
  
  putStrLn $ "Извлечение значения из rightValue: " ++ 
             show (fromMaybe 0 (preview _Right rightValue))
  
  let updatedRight = rightValue & _Right %~ (+10)
  putStrLn $ "rightValue после добавления 10: " ++ show updatedRight
  
  let updatedLeft = leftValue & _Left %~ map toUpper
  putStrLn $ "leftValue после преобразования в верхний регистр: " ++ show updatedLeft
  
  -- Создание новых значений через призмы
  let newJust = review _Just 100
  let newLeft = review _Left "new error"
  
  putStrLn $ "\nСозданные через призмы значения:"
  putStrLn $ "  newJust: " ++ show newJust
  putStrLn $ "  newLeft: " ++ show newLeft

-- Пример 3: Призмы для работы с типами-суммами
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Призмы для работы с типами-суммами"
  
  -- Определим тип данных для представления JSON-подобных значений
  let jsonValues = 
        [ JNull
        , JBool True
        , JNumber 42
        , JString "hello"
        , JArray [JNumber 1, JNumber 2, JNumber 3]
        , JObject $ Map.fromList [("name", JString "John"), ("age", JNumber 30)]
        ]
  
  -- Создадим призмы для каждого конструктора
  let _JNull = prism' 
        (\() -> JNull)
        (\j -> case j of
                JNull -> Just ()
                _ -> Nothing)
  
  let _JBool = prism 
        JBool
        (\j -> case j of
                JBool b -> Right b
                other -> Left other)
  
  let _JNumber = prism 
        JNumber
        (\j -> case j of
                JNumber n -> Right n
                other -> Left other)
  
  let _JString = prism 
        JString
        (\j -> case j of
                JString s -> Right s
                other -> Left other)
  
  let _JArray = prism 
        JArray
        (\j -> case j of
                JArray a -> Right a
                other -> Left other)
  
  let _JObject = prism 
        JObject
        (\j -> case j of
                JObject o -> Right o
                other -> Left other)
  
  -- Использование призм для фильтрации значений по типу
  let strings = filter (is _JString) jsonValues
  let numbers = filter (is _JNumber) jsonValues
  let arrays = filter (is _JArray) jsonValues
  
  putStrLn $ "Количество строк: " ++ show (length strings)
  putStrLn $ "Количество чисел: " ++ show (length numbers)
  putStrLn $ "Количество массивов: " ++ show (length arrays)
  
  -- Извлечение и обработка значений через призмы
  let stringValues = mapMaybe (preview _JString) jsonValues
  putStrLn $ "\nСтроковые значения: " ++ show stringValues
  
  let numberValues = mapMaybe (preview _JNumber) jsonValues
  putStrLn $ "Числовые значения: " ++ show numberValues
  
  -- Обновление значений через призмы
  let updatedValues = map (\j -> 
                            if is _JString j 
                            then j & _JString %~ map toUpper
                            else if is _JNumber j
                                 then j & _JNumber %~ (*2)
                                 else j) 
                          jsonValues
  
  putStrLn $ "\nОбновленные значения:"
  mapM_ (\j -> putStrLn $ "  " ++ show j) updatedValues
  
  -- Создание новых значений через призмы
  let newString = review _JString "new string"
  let newArray = review _JArray [JBool False, JString "item"]
  
  putStrLn $ "\nСозданные через призмы значения:"
  putStrLn $ "  newString: " ++ show newString
  putStrLn $ "  newArray: " ++ show newArray

-- Тип данных для примера 3
data JsonValue = JNull
               | JBool Bool
               | JNumber Int
               | JString String
               | JArray [JsonValue]
               | JObject (Map.Map String JsonValue)
               deriving (Show, Eq)

-- Часть 2: Траверсалы (Traversal)
-- Траверсалы - это оптики, которые фокусируются на нуле или более элементах
-- внутри структуры данных. Они позволяют обходить, извлекать и обновлять
-- несколько элементов одновременно.

-- Пример 4: Основы траверсалов
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Основы траверсалов"
  
  -- Определим некоторые структуры данных
  let numbers = [1, 2, 3, 4, 5]
  let nested = [[1, 2], [3, 4], [5, 6]]
  
  -- Траверсал для списка
  let listTraversal = traverse
  
  -- Получение всех элементов через траверсал
  putStrLn $ "Все элементы списка: " ++ show (numbers ^.. listTraversal)
  
  -- Обновление всех элементов через траверсал
  let doubledNumbers = numbers & listTraversal %~ (*2)
  putStrLn $ "Удвоенные числа: " ++ show doubledNumbers
  
  -- Фильтрация элементов через траверсал
  let evenNumbers = numbers ^.. listTraversal . filtered even
  putStrLn $ "Четные числа: " ++ show evenNumbers
  
  -- Траверсал для вложенных списков
  let nestedTraversal = traverse . traverse
  
  -- Получение всех элементов через вложенный траверсал
  putStrLn $ "\nВсе элементы вложенного списка: " ++ show (nested ^.. nestedTraversal)
  
  -- Обновление всех элементов через вложенный траверсал
  let doubledNested = nested & nestedTraversal %~ (*2)
  putStrLn $ "Удвоенные вложенные числа: " ++ show doubledNested
  
  -- Фильтрация элементов через вложенный траверсал
  let evenNestedNumbers = nested ^.. nestedTraversal . filtered even
  putStrLn $ "Четные числа во вложенном списке: " ++ show evenNestedNumbers
  
  -- Использование индексированного траверсала
  putStrLn $ "\nЭлементы с индексами:"
  let indexedList = numbers ^@.. traversed
  mapM_ (\(i, v) -> putStrLn $ "  " ++ show i ++ ": " ++ show v) indexedList
  
  -- Обновление элементов с учетом их индексов
  let numbersWithIndices = numbers & traversed %@~ (\i v -> v * i)
  putStrLn $ "Числа, умноженные на их индексы: " ++ show numbersWithIndices

-- Пример 5: Траверсалы для сложных структур данных
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Траверсалы для сложных структур данных"
  
  -- Определим структуру данных для представления дерева
  let tree = Node 1 
              [ Node 2 
                  [ Node 4 []
                  , Node 5 []
                  ]
              , Node 3 
                  [ Node 6 []
                  , Node 7 
                      [ Node 8 []
                      , Node 9 []
                      ]
                  ]
              ]
  
  -- Создадим траверсал для обхода всех узлов дерева
  let allNodes = cosmos
  
  -- Получение всех значений узлов
  let nodeValues = tree ^.. allNodes . nodeValueL
        where nodeValueL = lens nodeValue (\n v -> n { nodeValue = v })
  
  putStrLn $ "Все значения узлов: " ++ show nodeValues
  
  -- Подсчет количества узлов
  putStrLn $ "Количество узлов: " ++ show (length $ tree ^.. allNodes)
  
  -- Подсчет количества листьев (узлов без детей)
  let leaves = tree ^.. allNodes . filtered (\n -> null (children n))
  putStrLn $ "Количество листьев: " ++ show (length leaves)
  
  -- Обновление всех значений узлов
  let doubledTree = tree & allNodes . nodeValueL %~ (*2)
  
  putStrLn $ "\nЗначения узлов после удвоения:"
  let doubledNodeValues = doubledTree ^.. allNodes . nodeValueL
  putStrLn $ "  " ++ show doubledNodeValues
  
  -- Фильтрация узлов по значению
  let evenNodes = tree ^.. allNodes . filtered (\n -> even (nodeValue n))
  putStrLn $ "\nУзлы с четными значениями:"
  mapM_ (\n -> putStrLn $ "  " ++ show (nodeValue n)) evenNodes
  
  -- Обновление только четных значений
  let updatedTree = tree & allNodes . filtered (\n -> even (nodeValue n)) . nodeValueL %~ (+10)
  
  putStrLn $ "\nЗначения узлов после обновления четных значений:"
  let updatedNodeValues = updatedTree ^.. allNodes . nodeValueL
  putStrLn $ "  " ++ show updatedNodeValues

-- Тип данных для примера 5
data Tree a = Node 
  { nodeValue :: a
  , children :: [Tree a]
  } deriving (Show, Eq)

instance Plated (Tree a) where
  plate f (Node v cs) = Node v <$> traverse f cs

-- Пример 6: Комбинирование призм и траверсалов
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Комбинирование призм и траверсалов"
  
  -- Определим структуру данных
  let values = 
        [ Right 1
        , Left "error 1"
        , Right 2
        , Left "error 2"
        , Right 3
        ] :: [Either String Int]
  
  -- Создадим призму для Right
  let _Right = prism 
        Right
        (\e -> case e of
                Right b -> Right b
                Left a -> Left (Left a))
  
  -- Создадим призму для Left
  let _Left = prism 
        Left
        (\e -> case e of
                Left a -> Right a
                Right b -> Left (Right b))
  
  -- Комбинирование траверсала и призмы для работы только с Right значениями
  let rightValues = values ^.. traverse . _Right
  putStrLn $ "Значения Right: " ++ show rightValues
  
  -- Комбинирование траверсала и призмы для работы только с Left значениями
  let leftValues = values ^.. traverse . _Left
  putStrLn $ "Значения Left: " ++ show leftValues
  
  -- Обновление только Right значений
  let doubledRights = values & traverse . _Right %~ (*2)
  putStrLn $ "\nПосле удвоения Right значений:"
  mapM_ (putStrLn . ("  " ++) . show) doubledRights
  
  -- Обновление только Left значений
  let uppercaseLefts = values & traverse . _Left %~ map toUpper
  putStrLn $ "\nПосле преобразования Left значений в верхний регистр:"
  mapM_ (putStrLn . ("  " ++) . show) uppercaseLefts
  
  -- Фильтрация и обновление
  let evenRights = values & traverse . _Right . filtered even %~ (+10)
  putStrLn $ "\nПосле добавления 10 к четным Right значениям:"
  mapM_ (putStrLn . ("  " ++) . show) evenRights
  
  -- Создание более сложной структуры данных
  let nestedValues = 
        [ [Right 1, Left "error 1"]
        , [Right 2, Right 3]
        , [Left "error 2", Left "error 3"]
        ] :: [[Either String Int]]
  
  -- Комбинирование нескольких траверсалов и призмы
  let allRightValues = nestedValues ^.. traverse . traverse . _Right
  putStrLn $ "\nВсе Right значения во вложенной структуре: " ++ show allRightValues
  
  -- Обновление всех Right значений во вложенной структуре
  let doubledNestedRights = nestedValues & traverse . traverse . _Right %~ (*2)
  putStrLn $ "\nПосле удвоения всех Right значений во вложенной структуре:"
  mapM_ (putStrLn . ("  " ++) . show) doubledNestedRights

-- Главная функция
main :: IO ()
main = do
  putStrLn "Призмы и траверсалы в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о призмах и траверсалах:"
  putStrLn "1. Призмы - это оптики, которые фокусируются на одном конструкторе алгебраического типа данных"
  putStrLn "2. Призмы позволяют извлекать данные из значений определенного типа и создавать новые значения"
  putStrLn "3. Траверсалы - это оптики, которые фокусируются на нуле или более элементах внутри структуры данных"
  putStrLn "4. Траверсалы позволяют обходить, извлекать и обновлять несколько элементов одновременно"
  putStrLn "5. Призмы и траверсалы можно комбинировать для работы со сложными структурами данных"
  putStrLn "6. Библиотека lens предоставляет множество полезных функций для работы с призмами и траверсалами"
