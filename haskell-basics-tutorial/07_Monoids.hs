{-
  Моноиды в Haskell
  
  В этом файле мы рассмотрим моноиды - важную алгебраическую структуру,
  которая широко используется в функциональном программировании.
-}

module Main where

import Data.Monoid
import Data.Foldable (fold, foldMap)

-- Моноид - это тип с бинарной операцией и нейтральным элементом,
-- удовлетворяющий определенным законам.

-- Класс типов Monoid в Haskell:
--
-- class Semigroup a => Monoid a where
--   mempty :: a                  -- нейтральный элемент
--   mappend :: a -> a -> a       -- бинарная операция (наследуется от Semigroup)
--   mconcat :: [a] -> a          -- свертка списка (по умолчанию реализована через foldr mappend mempty)
--
-- Законы моноида:
-- 1. Ассоциативность: (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
-- 2. Нейтральный элемент слева: mempty `mappend` x = x
-- 3. Нейтральный элемент справа: x `mappend` mempty = x
--
-- Оператор (<>) - это инфиксный синоним для mappend:
-- x <> y = x `mappend` y

-- Часть 1: Стандартные моноиды в Haskell

-- Пример 1: Списки
-- Для списков операция mappend - это конкатенация (++), а mempty - пустой список []
listExample1 :: [Int]
listExample1 = [1, 2, 3] <> [4, 5, 6]  -- [1, 2, 3, 4, 5, 6]

listExample2 :: [Int]
listExample2 = mempty <> [1, 2, 3]  -- [1, 2, 3]

listExample3 :: [Int]
listExample3 = mconcat [[1, 2], [3, 4], [5, 6]]  -- [1, 2, 3, 4, 5, 6]

-- Пример 2: Sum - сложение чисел
sumExample1 :: Sum Int
sumExample1 = Sum 5 <> Sum 7  -- Sum 12

sumExample2 :: Sum Int
sumExample2 = mempty <> Sum 5  -- Sum 5

sumExample3 :: Sum Int
sumExample3 = mconcat [Sum 1, Sum 2, Sum 3, Sum 4]  -- Sum 10

-- Пример 3: Product - умножение чисел
productExample1 :: Product Int
productExample1 = Product 5 <> Product 7  -- Product 35

productExample2 :: Product Int
productExample2 = mempty <> Product 5  -- Product 5

productExample3 :: Product Int
productExample3 = mconcat [Product 1, Product 2, Product 3, Product 4]  -- Product 24

-- Пример 4: Any - логическое "или"
anyExample1 :: Any
anyExample1 = Any True <> Any False  -- Any True

anyExample2 :: Any
anyExample2 = mempty <> Any True  -- Any True

anyExample3 :: Any
anyExample3 = mconcat [Any False, Any False, Any True, Any False]  -- Any True

-- Пример 5: All - логическое "и"
allExample1 :: All
allExample1 = All True <> All False  -- All False

allExample2 :: All
allExample2 = mempty <> All True  -- All True

allExample3 :: All
allExample3 = mconcat [All True, All True, All True, All True]  -- All True

-- Пример 6: First - первое значение Just
firstExample1 :: First Int
firstExample1 = First (Just 5) <> First (Just 7)  -- First (Just 5)

firstExample2 :: First Int
firstExample2 = mempty <> First (Just 5)  -- First (Just 5)

firstExample3 :: First Int
firstExample3 = mconcat [First Nothing, First (Just 2), First (Just 3)]  -- First (Just 2)

-- Пример 7: Last - последнее значение Just
lastExample1 :: Last Int
lastExample1 = Last (Just 5) <> Last (Just 7)  -- Last (Just 7)

lastExample2 :: Last Int
lastExample2 = mempty <> Last (Just 5)  -- Last (Just 5)

lastExample3 :: Last Int
lastExample3 = mconcat [Last (Just 1), Last Nothing, Last (Just 3)]  -- Last (Just 3)

-- Пример 8: Endo - композиция функций
endoExample1 :: Endo Int
endoExample1 = Endo (+1) <> Endo (*2)  -- Endo (\x -> (x * 2) + 1)

endoExample2 :: Endo Int
endoExample2 = mempty <> Endo (+1)  -- Endo (+1)

endoExample3 :: Int
endoExample3 = appEndo (mconcat [Endo (+1), Endo (*2), Endo (^2)]) 3  -- ((3^2) * 2) + 1 = 19

-- Часть 2: Создание собственных моноидов

-- Пример 1: Моноид для объединения множеств
newtype UnionSet a = UnionSet { getSet :: [a] }
  deriving (Show, Eq)

instance Ord a => Semigroup (UnionSet a) where
  UnionSet xs <> UnionSet ys = UnionSet (xs `union` ys)
    where
      union :: Ord a => [a] -> [a] -> [a]
      union = foldr (\x acc -> if x `elem` acc then acc else x : acc)

instance Ord a => Monoid (UnionSet a) where
  mempty = UnionSet []

-- Пример 2: Моноид для статистики
data Stats = Stats
  { count :: Int
  , total :: Int
  , minValue :: Maybe Int
  , maxValue :: Maybe Int
  } deriving (Show, Eq)

instance Semigroup Stats where
  Stats c1 s1 min1 max1 <> Stats c2 s2 min2 max2 = Stats
    { count = c1 + c2
    , total = s1 + s2
    , minValue = case (min1, min2) of
        (Nothing, m) -> m
        (m, Nothing) -> m
        (Just m1, Just m2) -> Just (min m1 m2)
    , maxValue = case (max1, max2) of
        (Nothing, m) -> m
        (m, Nothing) -> m
        (Just m1, Just m2) -> Just (Prelude.max m1 m2)
    }

instance Monoid Stats where
  mempty = Stats 0 0 Nothing Nothing

-- Функция для создания статистики из одного числа
singletonStats :: Int -> Stats
singletonStats n = Stats 1 n (Just n) (Just n)

-- Пример 3: Моноид для валидации
data Validation e a = Error e | Success a
  deriving (Show, Eq)

instance Semigroup e => Semigroup (Validation e a) where
  Error e1 <> Error e2 = Error (e1 <> e2)
  Error e1 <> Success _ = Error e1
  Success _ <> Error e2 = Error e2
  Success a <> Success _ = Success a

instance Semigroup e => Monoid (Validation e a) where
  mempty = Success undefined

-- Часть 3: Использование моноидов

-- Пример 1: Свертка списка с использованием моноида
sumList :: [Int] -> Int
sumList = getSum . foldMap Sum

productList :: [Int] -> Int
productList = getProduct . foldMap Product

anyTrue :: [Bool] -> Bool
anyTrue = getAny . foldMap Any

allTrue :: [Bool] -> Bool
allTrue = getAll . foldMap All

-- Пример 2: Использование моноида для объединения результатов
data User = User
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Show, Eq)

data UserValidationError
  = NameError String
  | AgeError String
  | EmailError String
  deriving (Show, Eq)

instance Semigroup UserValidationError where
  NameError e1 <> NameError e2 = NameError (e1 <> "; " <> e2)
  AgeError e1 <> AgeError e2 = AgeError (e1 <> "; " <> e2)
  EmailError e1 <> EmailError e2 = EmailError (e1 <> "; " <> e2)
  e1 <> e2 = NameError (show e1 <> "; " <> show e2)

validateName :: String -> Validation UserValidationError String
validateName name
  | length name < 2 = Error (NameError "Имя слишком короткое")
  | otherwise = Success name

validateAge :: Int -> Validation UserValidationError Int
validateAge age
  | age < 0 = Error (AgeError "Возраст не может быть отрицательным")
  | age > 120 = Error (AgeError "Возраст слишком большой")
  | otherwise = Success age

validateEmail :: String -> Validation UserValidationError String
validateEmail email
  | not ('@' `elem` email) = Error (EmailError "Email должен содержать символ @")
  | otherwise = Success email

validateUser :: String -> Int -> String -> Validation UserValidationError User
validateUser name age email =
  case (validateName name, validateAge age, validateEmail email) of
    (Success n, Success a, Success e) -> Success (User n a e)
    (nameResult, ageResult, emailResult) ->
      let nameError = case nameResult of
                        Error e -> Error e
                        Success _ -> mempty
          ageError = case ageResult of
                       Error e -> Error e
                       Success _ -> mempty
          emailError = case emailResult of
                         Error e -> Error e
                         Success _ -> mempty
      in mconcat [nameError, ageError, emailError]

-- Пример 3: Использование моноида для композиции функций
applyFunctions :: [a -> a] -> a -> a
applyFunctions fs x = appEndo (foldMap Endo fs) x

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Стандартные моноиды в Haskell"
  
  putStrLn $ "Списки: [1, 2, 3] <> [4, 5, 6] = " ++ show listExample1
  putStrLn $ "Списки: mempty <> [1, 2, 3] = " ++ show listExample2
  putStrLn $ "Списки: mconcat [[1, 2], [3, 4], [5, 6]] = " ++ show listExample3
  
  putStrLn $ "Sum: Sum 5 <> Sum 7 = " ++ show sumExample1
  putStrLn $ "Sum: mempty <> Sum 5 = " ++ show sumExample2
  putStrLn $ "Sum: mconcat [Sum 1, Sum 2, Sum 3, Sum 4] = " ++ show sumExample3
  
  putStrLn $ "Product: Product 5 <> Product 7 = " ++ show productExample1
  putStrLn $ "Product: mempty <> Product 5 = " ++ show productExample2
  putStrLn $ "Product: mconcat [Product 1, Product 2, Product 3, Product 4] = " ++ show productExample3
  
  putStrLn $ "Any: Any True <> Any False = " ++ show anyExample1
  putStrLn $ "Any: mempty <> Any True = " ++ show anyExample2
  putStrLn $ "Any: mconcat [Any False, Any False, Any True, Any False] = " ++ show anyExample3
  
  putStrLn $ "All: All True <> All False = " ++ show allExample1
  putStrLn $ "All: mempty <> All True = " ++ show allExample2
  putStrLn $ "All: mconcat [All True, All True, All True, All True] = " ++ show allExample3
  
  putStrLn $ "First: First (Just 5) <> First (Just 7) = " ++ show firstExample1
  putStrLn $ "First: mempty <> First (Just 5) = " ++ show firstExample2
  putStrLn $ "First: mconcat [First Nothing, First (Just 2), First (Just 3)] = " ++ show firstExample3
  
  putStrLn $ "Last: Last (Just 5) <> Last (Just 7) = " ++ show lastExample1
  putStrLn $ "Last: mempty <> Last (Just 5) = " ++ show lastExample2
  putStrLn $ "Last: mconcat [Last (Just 1), Last Nothing, Last (Just 3)] = " ++ show lastExample3
  
  putStrLn $ "Endo: appEndo (Endo (+1) <> Endo (*2)) 3 = " ++ show (appEndo (Endo (+1) <> Endo (*2)) 3)
  putStrLn $ "Endo: appEndo (mempty <> Endo (+1)) 3 = " ++ show (appEndo (mempty <> Endo (+1)) 3)
  putStrLn $ "Endo: appEndo (mconcat [Endo (+1), Endo (*2), Endo (^2)]) 3 = " ++ show endoExample3

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Создание собственных моноидов"
  
  let set1 = UnionSet [1, 2, 3]
  let set2 = UnionSet [3, 4, 5]
  putStrLn $ "UnionSet: " ++ show set1 ++ " <> " ++ show set2 ++ " = " ++ show (set1 <> set2)
  putStrLn $ "UnionSet: mempty <> " ++ show set1 ++ " = " ++ show (mempty <> set1)
  putStrLn $ "UnionSet: mconcat [UnionSet [1, 2], UnionSet [2, 3], UnionSet [3, 4]] = " ++ 
    show (mconcat [UnionSet [1, 2], UnionSet [2, 3], UnionSet [3, 4]])
  
  let stats1 = singletonStats 5
  let stats2 = singletonStats 10
  let stats3 = singletonStats 3
  putStrLn $ "Stats: " ++ show stats1 ++ " <> " ++ show stats2 ++ " = " ++ show (stats1 <> stats2)
  putStrLn $ "Stats: mempty <> " ++ show stats1 ++ " = " ++ show (mempty <> stats1)
  putStrLn $ "Stats: mconcat [stats1, stats2, stats3] = " ++ show (mconcat [stats1, stats2, stats3])
  
  let validation1 = Error (NameError "Ошибка 1") :: Validation UserValidationError String
  let validation2 = Error (NameError "Ошибка 2") :: Validation UserValidationError String
  let validation3 = Success "OK" :: Validation UserValidationError String
  putStrLn $ "Validation: " ++ show validation1 ++ " <> " ++ show validation2 ++ " = " ++ show (validation1 <> validation2)
  putStrLn $ "Validation: " ++ show validation1 ++ " <> " ++ show validation3 ++ " = " ++ show (validation1 <> validation3)
  putStrLn $ "Validation: " ++ show validation3 ++ " <> " ++ show validation1 ++ " = " ++ show (validation3 <> validation1)

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Использование моноидов"
  
  putStrLn $ "sumList [1, 2, 3, 4, 5] = " ++ show (sumList [1, 2, 3, 4, 5])
  putStrLn $ "productList [1, 2, 3, 4, 5] = " ++ show (productList [1, 2, 3, 4, 5])
  putStrLn $ "anyTrue [False, False, True, False] = " ++ show (anyTrue [False, False, True, False])
  putStrLn $ "allTrue [True, True, True, True] = " ++ show (allTrue [True, True, True, True])
  putStrLn $ "allTrue [True, False, True, True] = " ++ show (allTrue [True, False, True, True])
  
  putStrLn $ "validateUser \"John\" 25 \"john@example.com\" = " ++ 
    show (validateUser "John" 25 "john@example.com")
  putStrLn $ "validateUser \"J\" (-5) \"john-example.com\" = " ++ 
    show (validateUser "J" (-5) "john-example.com")
  
  let functions = [(+1), (*2), (^2)]
  putStrLn $ "applyFunctions [(+1), (*2), (^2)] 3 = " ++ show (applyFunctions functions 3)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Моноиды в Haskell\n"
  
  example1
  example2
  example3
  
  putStrLn "\nКлючевые моменты о моноидах:"
  putStrLn "1. Моноид - это тип с бинарной операцией (<>) и нейтральным элементом (mempty)"
  putStrLn "2. Законы моноида: ассоциативность и нейтральный элемент слева и справа"
  putStrLn "3. Стандартные моноиды в Haskell: списки, Sum, Product, Any, All, First, Last, Endo и другие"
  putStrLn "4. Моноиды позволяют абстрагировать операцию свертки (fold) для различных типов данных"
  putStrLn "5. Моноиды полезны для объединения результатов, композиции функций и других операций"
  putStrLn "6. Моноиды можно комбинировать для создания более сложных моноидов"
  putStrLn "7. Функция mconcat позволяет объединить список моноидов в один"
  putStrLn "8. Моноиды широко используются в библиотеках для работы с эффектами, парсерами и другими абстракциями"
