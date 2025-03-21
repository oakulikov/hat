{-
  Maybe и Either монады в Haskell
  
  В этом файле мы рассмотрим подробнее монады Maybe и Either и их применение.
-}

module Main where

import Control.Monad (liftM, ap)

-- Монада Maybe
-- Maybe представляет вычисление, которое может завершиться неудачей
-- data Maybe a = Nothing | Just a

-- Реализация Monad для Maybe (уже встроена в Haskell)
{-
instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  (Just x) >>= f = f x
-}

-- Пример использования Maybe для безопасных вычислений
safeDivide :: Double -> Double -> Maybe Double
safeDivide _ 0 = Nothing
safeDivide x y = Just (x / y)

safeRoot :: Double -> Maybe Double
safeRoot x | x >= 0 = Just (sqrt x)
           | otherwise = Nothing

-- Цепочка вычислений с Maybe
complexCalculation :: Double -> Double -> Maybe Double
complexCalculation x y = do
  result1 <- safeDivide x y
  result2 <- safeRoot result1
  return (result2 * 2)

-- То же самое, но с использованием оператора >>=
complexCalculationWithBind :: Double -> Double -> Maybe Double
complexCalculationWithBind x y =
  safeDivide x y >>= \result1 ->
  safeRoot result1 >>= \result2 ->
  return (result2 * 2)

-- Использование функции maybe для обработки результата
processResult :: Maybe Double -> String
processResult = maybe "Ошибка вычисления" (\x -> "Результат: " ++ show x)

-- Монада Either
-- Either представляет вычисление, которое может завершиться ошибкой с информацией о причине
-- data Either e a = Left e | Right a

-- Реализация Monad для Either (уже встроена в Haskell)
{-
instance Monad (Either e) where
  return = Right
  (Left e) >>= _ = Left e
  (Right x) >>= f = f x
-}

-- Пример использования Either для вычислений с информацией об ошибках
safeDivideEither :: Double -> Double -> Either String Double
safeDivideEither _ 0 = Left "Деление на ноль"
safeDivideEither x y = Right (x / y)

safeRootEither :: Double -> Either String Double
safeRootEither x | x >= 0 = Right (sqrt x)
                 | otherwise = Left "Отрицательное число под корнем"

-- Цепочка вычислений с Either
complexCalculationEither :: Double -> Double -> Either String Double
complexCalculationEither x y = do
  result1 <- safeDivideEither x y
  result2 <- safeRootEither result1
  return (result2 * 2)

-- То же самое, но с использованием оператора >>=
complexCalculationEitherWithBind :: Double -> Double -> Either String Double
complexCalculationEitherWithBind x y =
  safeDivideEither x y >>= \result1 ->
  safeRootEither result1 >>= \result2 ->
  return (result2 * 2)

-- Использование функции either для обработки результата
processResultEither :: Either String Double -> String
processResultEither = either id (\x -> "Результат: " ++ show x)

-- Пример 1: Использование Maybe для безопасных вычислений
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Использование Maybe для безопасных вычислений"
  
  putStrLn $ "safeDivide 10 2 = " ++ show (safeDivide 10 2)
  putStrLn $ "safeDivide 10 0 = " ++ show (safeDivide 10 0)
  
  putStrLn $ "safeRoot 16 = " ++ show (safeRoot 16)
  putStrLn $ "safeRoot (-16) = " ++ show (safeRoot (-16))
  
  putStrLn $ "complexCalculation 16 2 = " ++ show (complexCalculation 16 2)
  putStrLn $ "complexCalculation 16 0 = " ++ show (complexCalculation 16 0)
  putStrLn $ "complexCalculation (-16) 2 = " ++ show (complexCalculation (-16) 2)
  
  putStrLn $ "processResult (complexCalculation 16 2) = " ++ processResult (complexCalculation 16 2)
  putStrLn $ "processResult (complexCalculation 16 0) = " ++ processResult (complexCalculation 16 0)

-- Пример 2: Использование Either для вычислений с информацией об ошибках
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Использование Either для вычислений с информацией об ошибках"
  
  putStrLn $ "safeDivideEither 10 2 = " ++ show (safeDivideEither 10 2)
  putStrLn $ "safeDivideEither 10 0 = " ++ show (safeDivideEither 10 0)
  
  putStrLn $ "safeRootEither 16 = " ++ show (safeRootEither 16)
  putStrLn $ "safeRootEither (-16) = " ++ show (safeRootEither (-16))
  
  putStrLn $ "complexCalculationEither 16 2 = " ++ show (complexCalculationEither 16 2)
  putStrLn $ "complexCalculationEither 16 0 = " ++ show (complexCalculationEither 16 0)
  putStrLn $ "complexCalculationEither (-16) 2 = " ++ show (complexCalculationEither (-16) 2)
  
  putStrLn $ "processResultEither (complexCalculationEither 16 2) = " ++ processResultEither (complexCalculationEither 16 2)
  putStrLn $ "processResultEither (complexCalculationEither 16 0) = " ++ processResultEither (complexCalculationEither 16 0)
  putStrLn $ "processResultEither (complexCalculationEither (-16) 2) = " ++ processResultEither (complexCalculationEither (-16) 2)

-- Пример 3: Сравнение Maybe и Either
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Сравнение Maybe и Either"
  
  putStrLn "Maybe:"
  putStrLn "- Преимущества:"
  putStrLn "  * Простота использования"
  putStrLn "  * Встроенная поддержка в Prelude"
  putStrLn "  * Хорошо подходит для случаев, когда причина ошибки не важна"
  putStrLn "- Недостатки:"
  putStrLn "  * Не предоставляет информацию о причине ошибки"
  
  putStrLn "\nEither:"
  putStrLn "- Преимущества:"
  putStrLn "  * Предоставляет информацию о причине ошибки"
  putStrLn "  * Более гибкий, чем Maybe"
  putStrLn "  * Позволяет обрабатывать различные типы ошибок"
  putStrLn "- Недостатки:"
  putStrLn "  * Более сложный в использовании"
  putStrLn "  * Требует явного указания типа ошибки"

-- Пример 4: Использование функций для работы с Maybe и Either
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Использование функций для работы с Maybe и Either"
  
  putStrLn "Функции для работы с Maybe:"
  putStrLn "- maybe :: b -> (a -> b) -> Maybe a -> b"
  putStrLn "  Применяет функцию к значению внутри Just или возвращает значение по умолчанию для Nothing"
  putStrLn $ "  Пример: maybe 0 (*2) (Just 5) = " ++ show (maybe 0 (*2) (Just 5))
  putStrLn $ "  Пример: maybe 0 (*2) Nothing = " ++ show (maybe 0 (*2) Nothing)
  
  putStrLn "\n- fromMaybe :: a -> Maybe a -> a"
  putStrLn "  Извлекает значение из Maybe или возвращает значение по умолчанию"
  putStrLn $ "  Пример: fromMaybe 0 (Just 5) = " ++ show (fromMaybe 0 (Just 5))
  putStrLn $ "  Пример: fromMaybe 0 Nothing = " ++ show (fromMaybe 0 Nothing)
  
  putStrLn "\nФункции для работы с Either:"
  putStrLn "- either :: (a -> c) -> (b -> c) -> Either a b -> c"
  putStrLn "  Применяет первую функцию к значению внутри Left или вторую функцию к значению внутри Right"
  putStrLn $ "  Пример: either length (*2) (Left \"error\") = " ++ show (either length (*2) (Left "error" :: Either String Int))
  putStrLn $ "  Пример: either length (*2) (Right 5) = " ++ show (either length (*2) (Right 5 :: Either String Int))
  
  putStrLn "\n- fromLeft :: a -> Either a b -> a"
  putStrLn "  Извлекает значение из Left или возвращает значение по умолчанию"
  putStrLn $ "  Пример: fromLeft \"default\" (Left \"error\") = " ++ fromLeft "default" (Left "error")
  putStrLn $ "  Пример: fromLeft \"default\" (Right 5) = " ++ fromLeft "default" (Right 5)
  
  putStrLn "\n- fromRight :: b -> Either a b -> b"
  putStrLn "  Извлекает значение из Right или возвращает значение по умолчанию"
  putStrLn $ "  Пример: fromRight 0 (Right 5) = " ++ show (fromRight 0 (Right 5))
  putStrLn $ "  Пример: fromRight 0 (Left \"error\") = " ++ show (fromRight 0 (Left "error"))

-- Вспомогательные функции (аналоги из Data.Either)
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just a) = a

fromLeft :: a -> Either a b -> a
fromLeft d (Left a) = a
fromLeft d _ = d

fromRight :: b -> Either a b -> b
fromRight d (Right b) = b
fromRight d _ = d

-- Главная функция
main :: IO ()
main = do
  putStrLn "Maybe и Either монады в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о Maybe и Either монадах:"
  putStrLn "1. Maybe представляет вычисление, которое может завершиться неудачей"
  putStrLn "2. Either представляет вычисление, которое может завершиться ошибкой с информацией о причине"
  putStrLn "3. Обе монады позволяют писать код, который корректно обрабатывает ошибки"
  putStrLn "4. Maybe проще в использовании, но не предоставляет информацию о причине ошибки"
  putStrLn "5. Either более гибкий, но требует явного указания типа ошибки"
