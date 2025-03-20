{-
  Законы функторов в Haskell
  
  Функторы должны удовлетворять определенным законам, чтобы обеспечить
  предсказуемое поведение. В этом файле мы рассмотрим эти законы и
  проверим их на примерах.
-}

module FunctorLaws where

import Data.Char (toUpper)

-- Напомним определение типового класса Functor:
-- 
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Законы функторов:
-- 
-- 1. Закон сохранения идентичности (Identity law):
--    fmap id = id
--    
--    Применение функции id к значению в контексте должно давать то же самое,
--    что и применение id к самому контексту.
--
-- 2. Закон композиции (Composition law):
--    fmap (f . g) = fmap f . fmap g
--    
--    Применение композиции функций должно давать тот же результат,
--    что и последовательное применение каждой функции.

-- Функция для проверки закона сохранения идентичности
checkIdentityLaw :: (Functor f, Eq (f a), Show (f a)) => f a -> Bool
checkIdentityLaw x = 
  let leftSide = fmap id x
      rightSide = id x
      result = leftSide == rightSide
  in if result
     then True
     else error $ "Закон сохранения идентичности нарушен: " ++ 
                  show leftSide ++ " /= " ++ show rightSide

-- Функция для проверки закона композиции
checkCompositionLaw :: (Functor f, Eq (f c), Show (f c)) => 
                       (b -> c) -> (a -> b) -> f a -> Bool
checkCompositionLaw f g x = 
  let leftSide = fmap (f . g) x
      rightSide = (fmap f . fmap g) x
      result = leftSide == rightSide
  in if result
     then True
     else error $ "Закон композиции нарушен: " ++ 
                  show leftSide ++ " /= " ++ show rightSide

-- Пример 1: Проверка законов для Maybe
maybeExample :: IO ()
maybeExample = do
  putStrLn "1. Проверка законов для Maybe:"
  
  let justValue = Just 42
  let nothingValue = Nothing :: Maybe Int
  
  -- Проверка закона сохранения идентичности
  putStrLn $ "  Закон сохранения идентичности для Just 42: " ++ 
             show (checkIdentityLaw justValue)
  putStrLn $ "  Закон сохранения идентичности для Nothing: " ++ 
             show (checkIdentityLaw nothingValue)
  
  -- Проверка закона композиции
  let f = (*2) :: Int -> Int
  let g = (+3) :: Int -> Int
  
  putStrLn $ "  Закон композиции для Just 42 с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g justValue)
  putStrLn $ "  Закон композиции для Nothing с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g nothingValue)

-- Пример 2: Проверка законов для списка
listExample :: IO ()
listExample = do
  putStrLn "\n2. Проверка законов для списка:"
  
  let list1 = [1, 2, 3, 4, 5]
  let list2 = [] :: [Int]
  
  -- Проверка закона сохранения идентичности
  putStrLn $ "  Закон сохранения идентичности для [1,2,3,4,5]: " ++ 
             show (checkIdentityLaw list1)
  putStrLn $ "  Закон сохранения идентичности для []: " ++ 
             show (checkIdentityLaw list2)
  
  -- Проверка закона композиции
  let f = (*2) :: Int -> Int
  let g = (+3) :: Int -> Int
  
  putStrLn $ "  Закон композиции для [1,2,3,4,5] с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g list1)
  putStrLn $ "  Закон композиции для [] с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g list2)

-- Пример 3: Проверка законов для Either
eitherExample :: IO ()
eitherExample = do
  putStrLn "\n3. Проверка законов для Either:"
  
  let rightValue = Right 42 :: Either String Int
  let leftValue = Left "error" :: Either String Int
  
  -- Проверка закона сохранения идентичности
  putStrLn $ "  Закон сохранения идентичности для Right 42: " ++ 
             show (checkIdentityLaw rightValue)
  putStrLn $ "  Закон сохранения идентичности для Left \"error\": " ++ 
             show (checkIdentityLaw leftValue)
  
  -- Проверка закона композиции
  let f = (*2) :: Int -> Int
  let g = (+3) :: Int -> Int
  
  putStrLn $ "  Закон композиции для Right 42 с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g rightValue)
  putStrLn $ "  Закон композиции для Left \"error\" с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g leftValue)

-- Пример 4: Неправильная реализация функтора
-- Этот пример показывает, что происходит, когда законы функторов нарушаются

-- Определим свой тип данных для строк
data StringBox a = EmptyBox | StringBox String
  deriving (Show, Eq)

-- Неправильная реализация Functor для StringBox
instance Functor StringBox where
  -- Нарушает оба закона
  fmap _ EmptyBox = EmptyBox
  fmap _ (StringBox _) = StringBox "constant"  -- Всегда возвращает одну и ту же строку

badFunctorExample :: IO ()
badFunctorExample = do
  putStrLn "\n4. Пример неправильной реализации функтора:"
  
  let badValue = StringBox "hello"
  
  -- Проверка закона сохранения идентичности
  putStrLn "  Проверка закона сохранения идентичности для StringBox:"
  putStrLn $ "    fmap id (StringBox \"hello\") = " ++ show (fmap id badValue)
  putStrLn $ "    id (StringBox \"hello\") = " ++ show (id badValue)
  putStrLn "  Закон нарушен, так как функция всегда игнорируется"
  
  -- Проверка закона композиции
  let f = reverse :: String -> String
  let g = map toUpper :: String -> String
  
  putStrLn "  Проверка закона композиции для StringBox:"
  putStrLn $ "    fmap (f . g) (StringBox \"hello\") = " ++ show (fmap (f . g) badValue)
  putStrLn $ "    (fmap f . fmap g) (StringBox \"hello\") = " ++ 
             show ((fmap f . fmap g) badValue)
  putStrLn "  Закон соблюдается случайно, так как обе стороны возвращают StringBox \"constant\""

-- Пример 5: Правильная реализация функтора для собственного типа данных
-- Определим свой тип данных
data Box a = Empty | Box a
  deriving (Show, Eq)

-- Правильная реализация Functor для Box
instance Functor Box where
  fmap _ Empty = Empty
  fmap f (Box x) = Box (f x)

boxExample :: IO ()
boxExample = do
  putStrLn "\n5. Пример правильной реализации функтора:"
  
  let boxValue = Box 10
  let emptyBox = Empty :: Box Int
  
  -- Проверка закона сохранения идентичности
  putStrLn $ "  Закон сохранения идентичности для Box 10: " ++ 
             show (checkIdentityLaw boxValue)
  putStrLn $ "  Закон сохранения идентичности для Empty: " ++ 
             show (checkIdentityLaw emptyBox)
  
  -- Проверка закона композиции
  let f = (*2) :: Int -> Int
  let g = (+3) :: Int -> Int
  
  putStrLn $ "  Закон композиции для Box 10 с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g boxValue)
  putStrLn $ "  Закон композиции для Empty с f = (*2), g = (+3): " ++ 
             show (checkCompositionLaw f g emptyBox)

-- Пример 6: Почему законы функторов важны
whyLawsMatter :: IO ()
whyLawsMatter = do
  putStrLn "\n6. Почему законы функторов важны:"
  
  putStrLn "  1. Предсказуемое поведение:"
  putStrLn "     Законы гарантируют, что функторы ведут себя предсказуемо"
  putStrLn "     и согласованно, что позволяет писать более надежный код."
  
  putStrLn "  2. Абстракция и переиспользование:"
  putStrLn "     Законы позволяют создавать абстрактные функции, которые"
  putStrLn "     работают с любым функтором, не зная его конкретной реализации."
  
  putStrLn "  3. Оптимизация:"
  putStrLn "     Компилятор может использовать законы для оптимизации кода,"
  putStrLn "     например, объединяя несколько вызовов fmap в один."
  
  putStrLn "  4. Рассуждение о коде:"
  putStrLn "     Законы позволяют рассуждать о коде и доказывать его корректность,"
  putStrLn "     что особенно важно в функциональном программировании."

-- Главная функция
main :: IO ()
main = do
  putStrLn "Законы функторов в Haskell\n"
  
  maybeExample
  listExample
  eitherExample
  badFunctorExample
  boxExample
  whyLawsMatter
  
  putStrLn "\nКлючевые моменты о законах функторов:"
  putStrLn "1. Закон сохранения идентичности: fmap id = id"
  putStrLn "2. Закон композиции: fmap (f . g) = fmap f . fmap g"
  putStrLn "3. Эти законы гарантируют, что функторы ведут себя предсказуемо"
  putStrLn "4. Нарушение законов может привести к непредсказуемому поведению"
  putStrLn "5. Правильная реализация функторов важна для создания надежного кода"
