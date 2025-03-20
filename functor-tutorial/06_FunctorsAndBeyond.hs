{-
  Функторы и другие абстракции в Haskell
  
  В этом файле мы рассмотрим связь функторов с другими абстракциями
  в Haskell, такими как аппликативные функторы и монады.
-}

module FunctorsAndBeyond where

import Control.Applicative (liftA2)
import Control.Monad (liftM)

-- Иерархия типовых классов в Haskell:
-- 
-- Functor -> Applicative -> Monad
--
-- Это означает, что:
-- - Каждый аппликативный функтор является функтором
-- - Каждая монада является аппликативным функтором

-- Напомним определение типового класса Functor:
-- 
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Определение типового класса Applicative:
-- 
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--
-- Обратите внимание, что Applicative требует, чтобы f был Functor

-- Определение типового класса Monad:
-- 
-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b
--
-- Обратите внимание, что Monad требует, чтобы m был Applicative

-- Пример 1: Сравнение возможностей функторов, аппликативных функторов и монад
-- Рассмотрим пример с Maybe

-- Функтор Maybe позволяет применить функцию к значению в контексте Maybe
functorExample :: IO ()
functorExample = do
  putStrLn "1. Функтор Maybe:"
  
  let justValue = Just 5
  let nothingValue = Nothing :: Maybe Int
  
  -- Применение функции к Maybe-значению с помощью fmap
  let doubled1 = fmap (*2) justValue
  let doubled2 = fmap (*2) nothingValue
  
  putStrLn $ "  fmap (*2) (Just 5) = " ++ show doubled1
  putStrLn $ "  fmap (*2) Nothing = " ++ show doubled2
  
  -- Ограничение: функтор не может комбинировать несколько контекстов
  putStrLn "  Ограничение: функтор не может комбинировать несколько контекстов"
  putStrLn "  Например, мы не можем напрямую применить (+) к двум Maybe-значениям"

-- Аппликативный функтор Maybe позволяет применить функцию в контексте Maybe
-- к значению в контексте Maybe
applicativeExample :: IO ()
applicativeExample = do
  putStrLn "\n2. Аппликативный функтор Maybe:"
  
  let justValue1 = Just 5
  let justValue2 = Just 7
  let nothingValue = Nothing :: Maybe Int
  
  -- Применение функции в контексте к значению в контексте
  let result1 = Just (+) <*> justValue1 <*> justValue2
  let result2 = Just (+) <*> justValue1 <*> nothingValue
  let result3 = Just (+) <*> nothingValue <*> justValue2
  
  putStrLn $ "  Just (+) <*> Just 5 <*> Just 7 = " ++ show result1
  putStrLn $ "  Just (+) <*> Just 5 <*> Nothing = " ++ show result2
  putStrLn $ "  Just (+) <*> Nothing <*> Just 7 = " ++ show result3
  
  -- Более удобный способ с использованием liftA2
  let sum1 = liftA2 (+) justValue1 justValue2
  let sum2 = liftA2 (+) justValue1 nothingValue
  
  putStrLn $ "  liftA2 (+) (Just 5) (Just 7) = " ++ show sum1
  putStrLn $ "  liftA2 (+) (Just 5) Nothing = " ++ show sum2
  
  -- Ограничение: аппликативный функтор не может использовать результат
  -- предыдущего вычисления для определения следующего вычисления
  putStrLn "  Ограничение: аппликативный функтор не может использовать результат"
  putStrLn "  предыдущего вычисления для определения следующего вычисления"

-- Монада Maybe позволяет использовать результат предыдущего вычисления
-- для определения следующего вычисления
monadExample :: IO ()
monadExample = do
  putStrLn "\n3. Монада Maybe:"
  
  let justValue = Just 5
  let nothingValue = Nothing :: Maybe Int
  
  -- Использование результата предыдущего вычисления
  let result1 = justValue >>= (\x -> if x > 0 then Just (x * 2) else Nothing)
  let result2 = nothingValue >>= (\x -> if x > 0 then Just (x * 2) else Nothing)
  
  putStrLn $ "  Just 5 >>= (\\x -> if x > 0 then Just (x * 2) else Nothing) = " ++ show result1
  putStrLn $ "  Nothing >>= (\\x -> if x > 0 then Just (x * 2) else Nothing) = " ++ show result2
  
  -- То же самое с использованием do-нотации
  let computation1 = do
        x <- justValue
        if x > 0 then Just (x * 2) else Nothing
  
  let computation2 = do
        x <- nothingValue
        if x > 0 then Just (x * 2) else Nothing
  
  putStrLn $ "  do { x <- Just 5; if x > 0 then Just (x * 2) else Nothing } = " ++ show computation1
  putStrLn $ "  do { x <- Nothing; if x > 0 then Just (x * 2) else Nothing } = " ++ show computation2

-- Пример 2: Реализация Applicative и Monad через Functor
-- Рассмотрим простой тип данных Identity

-- Тип данных Identity
newtype Identity a = Identity { runIdentity :: a }
  deriving (Show, Eq)

-- Реализация Functor для Identity
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

-- Реализация Applicative для Identity через Functor
instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

-- Реализация Monad для Identity через Applicative
instance Monad Identity where
  return = pure
  Identity x >>= f = f x

-- Демонстрация реализации
implementationExample :: IO ()
implementationExample = do
  putStrLn "\n4. Реализация Applicative и Monad через Functor:"
  
  let value = Identity 5
  
  -- Использование функтора
  let doubled = fmap (*2) value
  putStrLn $ "  fmap (*2) (Identity 5) = " ++ show doubled
  
  -- Использование аппликативного функтора
  let applied = pure (+) <*> value <*> doubled
  putStrLn $ "  pure (+) <*> Identity 5 <*> Identity 10 = " ++ show applied
  
  -- Использование монады
  let bound = value >>= (\x -> return (x * 3))
  putStrLn $ "  Identity 5 >>= (\\x -> return (x * 3)) = " ++ show bound
  
  -- Демонстрация связи между fmap, <*> и >>=
  let fmapResult = fmap (*2) value
  let apResult = pure (*2) <*> value
  let bindResult = value >>= (\x -> return (x * 2))
  
  putStrLn $ "  fmap (*2) (Identity 5) = " ++ show fmapResult
  putStrLn $ "  pure (*2) <*> Identity 5 = " ++ show apResult
  putStrLn $ "  Identity 5 >>= (\\x -> return (x * 2)) = " ++ show bindResult
  putStrLn $ "  Все три выражения дают одинаковый результат: " ++ show (fmapResult == apResult && apResult == bindResult)

-- Пример 3: Реализация fmap через <*> и >>=
-- Мы можем реализовать fmap через <*> или >>=

-- Реализация fmap через <*>
fmapViaAp :: Applicative f => (a -> b) -> f a -> f b
fmapViaAp f x = pure f <*> x

-- Реализация fmap через >>=
fmapViaBind :: Monad m => (a -> b) -> m a -> m b
fmapViaBind f x = x >>= (return . f)

-- Демонстрация реализации fmap через <*> и >>=
fmapImplementationExample :: IO ()
fmapImplementationExample = do
  putStrLn "\n5. Реализация fmap через <*> и >>=:"
  
  let justValue = Just 5
  let listValue = [1, 2, 3]
  
  -- Реализация fmap через <*>
  let doubledViaAp1 = fmapViaAp (*2) justValue
  let doubledViaAp2 = fmapViaAp (*2) listValue
  
  putStrLn $ "  fmapViaAp (*2) (Just 5) = " ++ show doubledViaAp1
  putStrLn $ "  fmapViaAp (*2) [1, 2, 3] = " ++ show doubledViaAp2
  
  -- Реализация fmap через >>=
  let doubledViaBind1 = fmapViaBind (*2) justValue
  let doubledViaBind2 = fmapViaBind (*2) listValue
  
  putStrLn $ "  fmapViaBind (*2) (Just 5) = " ++ show doubledViaBind1
  putStrLn $ "  fmapViaBind (*2) [1, 2, 3] = " ++ show doubledViaBind2
  
  -- Сравнение с обычным fmap
  let doubledViaFmap1 = fmap (*2) justValue
  let doubledViaFmap2 = fmap (*2) listValue
  
  putStrLn $ "  fmap (*2) (Just 5) = " ++ show doubledViaFmap1
  putStrLn $ "  fmap (*2) [1, 2, 3] = " ++ show doubledViaFmap2
  
  -- Проверка эквивалентности
  putStrLn $ "  Все реализации эквивалентны для Maybe: " ++ 
             show (doubledViaAp1 == doubledViaBind1 && doubledViaBind1 == doubledViaFmap1)
  putStrLn $ "  Все реализации эквивалентны для списка: " ++ 
             show (doubledViaAp2 == doubledViaBind2 && doubledViaBind2 == doubledViaFmap2)

-- Пример 4: Когда использовать каждую абстракцию
whenToUseExample :: IO ()
whenToUseExample = do
  putStrLn "\n6. Когда использовать каждую абстракцию:"
  
  putStrLn "  Functor (fmap):"
  putStrLn "  - Когда нужно применить функцию к значению в контексте"
  putStrLn "  - Когда нужно преобразовать данные, не меняя контекст"
  putStrLn "  - Когда нужно работать с самой простой абстракцией"
  
  putStrLn "\n  Applicative (<*>):"
  putStrLn "  - Когда нужно применить функцию с несколькими аргументами к значениям в контексте"
  putStrLn "  - Когда вычисления не зависят друг от друга"
  putStrLn "  - Когда нужно комбинировать независимые вычисления"
  
  putStrLn "\n  Monad (>>=):"
  putStrLn "  - Когда каждое следующее вычисление зависит от результата предыдущего"
  putStrLn "  - Когда нужно использовать условную логику на основе предыдущих результатов"
  putStrLn "  - Когда нужна максимальная гибкость в управлении вычислениями"

-- Пример 5: Практическое сравнение на примере валидации данных
-- Рассмотрим пример валидации данных с использованием разных абстракций

-- Тип данных для результата валидации
data Validation e a = Failure e | Success a
  deriving (Show, Eq)

-- Реализация Functor для Validation
instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

-- Реализация Applicative для Validation
-- Особенность: накапливает ошибки
instance Monoid e => Applicative (Validation e) where
  pure = Success
  
  Failure e1 <*> Failure e2 = Failure (e1 `mappend` e2)
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e
  Success f <*> Success a = Success (f a)

-- Реализация Monad для Validation
-- Особенность: останавливается на первой ошибке
instance Monoid e => Monad (Validation e) where
  return = pure
  
  Failure e >>= _ = Failure e
  Success a >>= f = f a

-- Функции для валидации
validateName :: String -> Validation [String] String
validateName name
  | length name < 2 = Failure ["Имя должно содержать не менее 2 символов"]
  | length name > 50 = Failure ["Имя должно содержать не более 50 символов"]
  | otherwise = Success name

validateAge :: Int -> Validation [String] Int
validateAge age
  | age < 18 = Failure ["Возраст должен быть не менее 18 лет"]
  | age > 120 = Failure ["Возраст должен быть не более 120 лет"]
  | otherwise = Success age

validateEmail :: String -> Validation [String] String
validateEmail email
  | '@' `notElem` email = Failure ["Email должен содержать символ @"]
  | length email < 5 = Failure ["Email слишком короткий"]
  | otherwise = Success email

-- Тип данных для пользователя
data User = User {
  userName :: String,
  userAge :: Int,
  userEmail :: String
} deriving (Show, Eq)

-- Валидация с использованием функтора (ограниченные возможности)
validateUserFunctor :: String -> Int -> String -> Validation [String] User
validateUserFunctor name age email =
  case validateName name of
    Failure e1 -> Failure e1
    Success validName ->
      case validateAge age of
        Failure e2 -> Failure e2
        Success validAge ->
          case validateEmail email of
            Failure e3 -> Failure e3
            Success validEmail ->
              Success (User validName validAge validEmail)

-- Валидация с использованием аппликативного функтора (накапливает все ошибки)
validateUserApplicative :: String -> Int -> String -> Validation [String] User
validateUserApplicative name age email =
  User <$> validateName name <*> validateAge age <*> validateEmail email

-- Валидация с использованием монады (останавливается на первой ошибке)
validateUserMonad :: String -> Int -> String -> Validation [String] User
validateUserMonad name age email = do
  validName <- validateName name
  validAge <- validateAge age
  validEmail <- validateEmail email
  return (User validName validAge validEmail)

-- Демонстрация валидации
validationExample :: IO ()
validationExample = do
  putStrLn "\n7. Практическое сравнение на примере валидации данных:"
  
  -- Валидные данные
  let validName = "John Doe"
  let validAge = 30
  let validEmail = "john@example.com"
  
  -- Невалидные данные
  let invalidName = "J"
  let invalidAge = 15
  let invalidEmail = "not-an-email"
  
  -- Валидация с использованием функтора
  putStrLn "  Валидация с использованием функтора (ограниченные возможности):"
  putStrLn $ "  Валидные данные: " ++ show (validateUserFunctor validName validAge validEmail)
  putStrLn $ "  Невалидные данные: " ++ show (validateUserFunctor invalidName invalidAge invalidEmail)
  
  -- Валидация с использованием аппликативного функтора
  putStrLn "\n  Валидация с использованием аппликативного функтора (накапливает все ошибки):"
  putStrLn $ "  Валидные данные: " ++ show (validateUserApplicative validName validAge validEmail)
  putStrLn $ "  Невалидные данные: " ++ show (validateUserApplicative invalidName invalidAge invalidEmail)
  
  -- Валидация с использованием монады
  putStrLn "\n  Валидация с использованием монады (останавливается на первой ошибке):"
  putStrLn $ "  Валидные данные: " ++ show (validateUserMonad validName validAge validEmail)
  putStrLn $ "  Невалидные данные: " ++ show (validateUserMonad invalidName invalidAge invalidEmail)
  
  -- Объяснение различий
  putStrLn "\n  Различия между подходами:"
  putStrLn "  - Функтор: самый простой подход, но требует ручной обработки ошибок"
  putStrLn "  - Аппликативный функтор: накапливает все ошибки, что полезно для валидации форм"
  putStrLn "  - Монада: останавливается на первой ошибке, что может быть эффективнее"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Функторы и другие абстракции в Haskell\n"
  
  functorExample
  applicativeExample
  monadExample
  implementationExample
  fmapImplementationExample
  whenToUseExample
  validationExample
  
  putStrLn "\nКлючевые моменты о функторах и других абстракциях:"
  putStrLn "1. Иерархия типовых классов: Functor -> Applicative -> Monad"
  putStrLn "2. Функтор позволяет применить функцию к значению в контексте"
  putStrLn "3. Аппликативный функтор позволяет применить функцию в контексте к значению в контексте"
  putStrLn "4. Монада позволяет использовать результат предыдущего вычисления для определения следующего"
  putStrLn "5. fmap можно реализовать через <*> или >>="
  putStrLn "6. Выбор абстракции зависит от конкретной задачи"
  putStrLn "7. Аппликативные функторы и монады могут по-разному обрабатывать ошибки"
