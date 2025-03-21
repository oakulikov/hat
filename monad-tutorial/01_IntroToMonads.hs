{-
  Введение в монады в Haskell
  
  В этом файле мы рассмотрим основы монад в Haskell и их применение.
-}

module Main where

import Control.Monad (liftM, ap)

-- Монада - это тип данных, который реализует два метода:
-- 1. return (или pure) - помещает значение в монадический контекст
-- 2. >>= (bind) - позволяет применить функцию к значению в монадическом контексте

-- Класс типов Monad в Haskell определен примерно так:
{-
class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
-}

-- Рассмотрим простейшую монаду - Maybe
-- Maybe представляет вычисление, которое может завершиться неудачей

-- Пример использования Maybe без монадического стиля
divideNonMonadic :: Double -> Double -> Maybe Double
divideNonMonadic _ 0 = Nothing
divideNonMonadic x y = Just (x / y)

-- Цепочка вычислений без монад
complexDivisionNonMonadic :: Double -> Double -> Double -> Maybe Double
complexDivisionNonMonadic x y z =
  case divideNonMonadic x y of
    Nothing -> Nothing
    Just r1 -> case divideNonMonadic r1 z of
      Nothing -> Nothing
      Just r2 -> Just (r2 * 2)

-- Тот же пример с использованием монадического стиля
divideMonadic :: Double -> Double -> Maybe Double
divideMonadic _ 0 = Nothing
divideMonadic x y = Just (x / y)

-- Цепочка вычислений с монадами
complexDivisionMonadic :: Double -> Double -> Double -> Maybe Double
complexDivisionMonadic x y z = do
  r1 <- divideMonadic x y
  r2 <- divideMonadic r1 z
  return (r2 * 2)

-- То же самое, но с использованием оператора >>=
complexDivisionWithBind :: Double -> Double -> Double -> Maybe Double
complexDivisionWithBind x y z =
  divideMonadic x y >>= \r1 ->
  divideMonadic r1 z >>= \r2 ->
  return (r2 * 2)

-- Создадим свою монаду для примера
-- Монада Writer - позволяет логировать сообщения в процессе вычислений
newtype Writer log a = Writer { runWriter :: (a, log) }

-- Реализация Functor для Writer
instance Functor (Writer log) where
  fmap f (Writer (x, log)) = Writer (f x, log)

-- Реализация Applicative для Writer
instance Monoid log => Applicative (Writer log) where
  pure x = Writer (x, mempty)
  (<*>) = ap

-- Реализация Monad для Writer
instance Monoid log => Monad (Writer log) where
  return = pure
  Writer (x, log) >>= f =
    let Writer (y, log') = f x
    in Writer (y, log `mappend` log')

-- Функция для логирования сообщений
tell :: log -> Writer log ()
tell log = Writer ((), log)

-- Пример использования Writer
logDivision :: Double -> Double -> Writer [String] (Maybe Double)
logDivision x y = do
  tell ["Dividing " ++ show x ++ " by " ++ show y]
  if y == 0
    then do
      tell ["Division by zero!"]
      return Nothing
    else do
      tell ["Result: " ++ show (x / y)]
      return (Just (x / y))

-- Монада State - позволяет работать с изменяемым состоянием
newtype State s a = State { runState :: s -> (a, s) }

-- Реализация Functor для State
instance Functor (State s) where
  fmap f (State g) = State $ \s ->
    let (a, s') = g s
    in (f a, s')

-- Реализация Applicative для State
instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (<*>) = ap

-- Реализация Monad для State
instance Monad (State s) where
  return = pure
  State act >>= f = State $ \s ->
    let (a, s') = act s
        State act' = f a
    in act' s'

-- Функции для работы с состоянием
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- Пример использования State
countDown :: State Int [Int]
countDown = do
  n <- get
  if n <= 0
    then return []
    else do
      put (n - 1)
      rest <- countDown
      return (n : rest)

-- Монада IO - позволяет выполнять ввод-вывод
-- IO уже встроена в Haskell, поэтому мы просто покажем примеры использования

-- Пример использования IO
greet :: IO ()
greet = do
  putStrLn "Как вас зовут?"
  name <- getLine
  putStrLn $ "Привет, " ++ name ++ "!"

-- Пример 1: Использование Maybe
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Монада Maybe"
  
  putStrLn $ "Деление 10 на 2 без монад: " ++ show (complexDivisionNonMonadic 10 2 5)
  putStrLn $ "Деление 10 на 0 без монад: " ++ show (complexDivisionNonMonadic 10 0 5)
  
  putStrLn $ "Деление 10 на 2 с монадами: " ++ show (complexDivisionMonadic 10 2 5)
  putStrLn $ "Деление 10 на 0 с монадами: " ++ show (complexDivisionMonadic 10 0 5)
  
  putStrLn $ "Деление 10 на 2 с bind: " ++ show (complexDivisionWithBind 10 2 5)
  putStrLn $ "Деление 10 на 0 с bind: " ++ show (complexDivisionWithBind 10 0 5)

-- Пример 2: Использование Writer
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Монада Writer"
  
  let (result1, logs1) = runWriter (logDivision 10 2)
  putStrLn "Деление 10 на 2:"
  mapM_ putStrLn logs1
  putStrLn $ "Результат: " ++ show result1
  
  let (result2, logs2) = runWriter (logDivision 10 0)
  putStrLn "\nДеление 10 на 0:"
  mapM_ putStrLn logs2
  putStrLn $ "Результат: " ++ show result2

-- Пример 3: Использование State
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Монада State"
  
  let (result, finalState) = runState countDown 5
  putStrLn $ "Обратный отсчет от 5: " ++ show result
  putStrLn $ "Конечное состояние: " ++ show finalState
  
  -- Еще один пример с State - подсчет факториала
  let factorial n = 
        if n <= 1
          then State $ \s -> (s, s)
          else do
            modify (* n)
            factorial (n - 1)
  
  let (result3, _) = runState (factorial 5) 1
  putStrLn $ "Факториал 5: " ++ show result3

-- Пример 4: Использование IO
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Монада IO"
  putStrLn "Монада IO позволяет выполнять операции ввода-вывода в чистом функциональном языке."
  putStrLn "Пример функции greet:"
  putStrLn "greet = do"
  putStrLn "  putStrLn \"Как вас зовут?\""
  putStrLn "  name <- getLine"
  putStrLn "  putStrLn $ \"Привет, \" ++ name ++ \"!\""
  
  -- Мы не будем вызывать greet здесь, чтобы не прерывать вывод примеров

-- Пример 5: Законы монад
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Законы монад"
  putStrLn "Все монады должны удовлетворять трем законам:"
  
  putStrLn "1. Левая единица: return a >>= f ≡ f a"
  putStrLn "   Если мы помещаем значение в монаду с помощью return и затем применяем к нему функцию f,"
  putStrLn "   результат должен быть таким же, как если бы мы просто применили f к значению."
  
  putStrLn "2. Правая единица: m >>= return ≡ m"
  putStrLn "   Если мы берем монадическое значение и применяем к нему функцию return,"
  putStrLn "   результат должен быть таким же, как исходное монадическое значение."
  
  putStrLn "3. Ассоциативность: (m >>= f) >>= g ≡ m >>= (\\x -> f x >>= g)"
  putStrLn "   Порядок вложенности операций bind не должен влиять на результат."

-- Пример 6: Монадические функции
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Монадические функции"
  
  putStrLn "В библиотеке Control.Monad есть много полезных функций для работы с монадами:"
  
  putStrLn "1. liftM :: Monad m => (a -> b) -> m a -> m b"
  putStrLn "   Применяет функцию к значению внутри монады."
  
  putStrLn "2. sequence :: Monad m => [m a] -> m [a]"
  putStrLn "   Преобразует список монадических значений в монадическое значение списка."
  
  putStrLn "3. mapM :: Monad m => (a -> m b) -> [a] -> m [b]"
  putStrLn "   Применяет монадическую функцию к каждому элементу списка и собирает результаты."
  
  putStrLn "4. filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]"
  putStrLn "   Фильтрует список с помощью монадического предиката."
  
  putStrLn "5. foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b"
  putStrLn "   Свертка списка с помощью монадической функции."

-- Главная функция
main :: IO ()
main = do
  putStrLn "Монады в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о монадах:"
  putStrLn "1. Монада - это тип данных, который реализует методы return и >>= (bind)"
  putStrLn "2. Монады позволяют абстрагировать и инкапсулировать различные эффекты вычислений"
  putStrLn "3. Монады делают код более читаемым и модульным"
  putStrLn "4. Основные монады в Haskell: Maybe, Either, List, IO, State, Reader, Writer"
  putStrLn "5. Монады должны удовлетворять трем законам: левая единица, правая единица и ассоциативность"
