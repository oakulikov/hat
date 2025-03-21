{-
  State и Reader монады в Haskell
  
  В этом файле мы рассмотрим подробнее монады State и Reader и их применение.
-}

module Main where

import Control.Monad (liftM, ap)
import Control.Monad.State (State, runState, get, put, modify, evalState, execState)
import Control.Monad.Reader (Reader, runReader, ask, local, asks)

-- Монада State
-- State представляет вычисление с изменяемым состоянием
-- newtype State s a = State { runState :: s -> (a, s) }

-- Реализация Monad для State (уже встроена в Control.Monad.State)
{-
instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State act >>= f = State $ \s ->
    let (a, s') = act s
        State act' = f a
    in act' s'
-}

-- Пример использования State для подсчета факториала
factorial :: Int -> State Int Int
factorial n = do
  if n <= 1
    then get
    else do
      modify (* n)
      factorial (n - 1)

-- Пример использования State для обхода дерева
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

-- Функция для нумерации узлов дерева
numberTree :: Tree a -> State Int (Tree (a, Int))
numberTree (Leaf x) = do
  n <- get
  put (n + 1)
  return (Leaf (x, n))
numberTree (Node left right) = do
  left' <- numberTree left
  right' <- numberTree right
  return (Node left' right')

-- Пример использования State для игры
data GameState = GameState
  { score :: Int
  , level :: Int
  , lives :: Int
  } deriving (Show)

initialState :: GameState
initialState = GameState { score = 0, level = 1, lives = 3 }

-- Функции для изменения состояния игры
addScore :: Int -> State GameState ()
addScore points = modify $ \s -> s { score = score s + points }

levelUp :: State GameState ()
levelUp = modify $ \s -> s { level = level s + 1 }

loseLife :: State GameState ()
loseLife = modify $ \s -> s { lives = lives s - 1 }

-- Игровая логика
playGame :: State GameState String
playGame = do
  addScore 100
  levelUp
  addScore 200
  loseLife
  addScore 300
  state <- get
  return $ "Game over! Final score: " ++ show (score state)

-- Монада Reader
-- Reader представляет вычисление с доступом к окружению
-- newtype Reader r a = Reader { runReader :: r -> a }

-- Реализация Monad для Reader (уже встроена в Control.Monad.Reader)
{-
instance Monad (Reader r) where
  return a = Reader $ \_ -> a
  Reader f >>= g = Reader $ \r -> runReader (g (f r)) r
-}

-- Пример использования Reader для доступа к конфигурации
data Config = Config
  { dbHost :: String
  , dbPort :: Int
  , dbUser :: String
  , dbPassword :: String
  , appName :: String
  , appVersion :: String
  } deriving (Show)

-- Функция для получения строки подключения к базе данных
getConnectionString :: Reader Config String
getConnectionString = do
  config <- ask
  return $ "host=" ++ dbHost config ++
           " port=" ++ show (dbPort config) ++
           " user=" ++ dbUser config ++
           " password=" ++ dbPassword config

-- Функция для получения информации о приложении
getAppInfo :: Reader Config String
getAppInfo = do
  config <- ask
  return $ appName config ++ " v" ++ appVersion config

-- Функция для получения полной информации
getFullInfo :: Reader Config String
getFullInfo = do
  connStr <- getConnectionString
  appInfo <- getAppInfo
  return $ "App: " ++ appInfo ++ "\nConnection: " ++ connStr

-- Функция для получения информации с измененной конфигурацией
getSecureInfo :: Reader Config String
getSecureInfo = do
  info <- local (\c -> c { dbPassword = "********" }) getFullInfo
  return info

-- Функция для получения только имени хоста
getHostName :: Reader Config String
getHostName = asks dbHost

-- Пример 1: Использование State для подсчета факториала
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Использование State для подсчета факториала"
  
  let (result, finalState) = runState (factorial 5) 1
  putStrLn $ "Факториал 5 = " ++ show result
  putStrLn $ "Конечное состояние = " ++ show finalState
  
  -- Использование evalState (возвращает только результат)
  putStrLn $ "evalState (factorial 5) 1 = " ++ show (evalState (factorial 5) 1)
  
  -- Использование execState (возвращает только конечное состояние)
  putStrLn $ "execState (factorial 5) 1 = " ++ show (execState (factorial 5) 1)

-- Пример 2: Использование State для обхода дерева
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Использование State для обхода дерева"
  
  let tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
  let (numberedTree, finalCount) = runState (numberTree tree) 1
  
  putStrLn $ "Исходное дерево: " ++ show tree
  putStrLn $ "Пронумерованное дерево: " ++ show numberedTree
  putStrLn $ "Конечное состояние счетчика: " ++ show finalCount

-- Пример 3: Использование State для игры
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Использование State для игры"
  
  let (result, finalState) = runState playGame initialState
  
  putStrLn $ "Начальное состояние: " ++ show initialState
  putStrLn $ "Конечное состояние: " ++ show finalState
  putStrLn $ "Результат: " ++ result

-- Пример 4: Использование Reader для доступа к конфигурации
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Использование Reader для доступа к конфигурации"
  
  let config = Config
        { dbHost = "localhost"
        , dbPort = 5432
        , dbUser = "admin"
        , dbPassword = "secret"
        , appName = "MyApp"
        , appVersion = "1.0.0"
        }
  
  putStrLn $ "Конфигурация: " ++ show config
  putStrLn $ "Строка подключения: " ++ runReader getConnectionString config
  putStrLn $ "Информация о приложении: " ++ runReader getAppInfo config
  putStrLn $ "Полная информация: " ++ runReader getFullInfo config
  putStrLn $ "Безопасная информация: " ++ runReader getSecureInfo config
  putStrLn $ "Имя хоста: " ++ runReader getHostName config

-- Пример 5: Сравнение State и Reader
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Сравнение State и Reader"
  
  putStrLn "State:"
  putStrLn "- Представляет вычисление с изменяемым состоянием"
  putStrLn "- Позволяет читать и изменять состояние"
  putStrLn "- Используется, когда состояние меняется в процессе вычислений"
  putStrLn "- Функции: get, put, modify"
  
  putStrLn "\nReader:"
  putStrLn "- Представляет вычисление с доступом к окружению"
  putStrLn "- Позволяет только читать окружение, но не изменять его"
  putStrLn "- Используется, когда окружение не меняется в процессе вычислений"
  putStrLn "- Функции: ask, asks, local"

-- Пример 6: Комбинирование State и Reader
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Комбинирование State и Reader"
  
  putStrLn "Для комбинирования монад State и Reader можно использовать монадные трансформеры:"
  putStrLn "- StateT - трансформер для State"
  putStrLn "- ReaderT - трансформер для Reader"
  
  putStrLn "\nПример типа, комбинирующего State и Reader:"
  putStrLn "type AppMonad r s a = StateT s (Reader r) a"
  
  putStrLn "\nТакой тип позволяет иметь доступ к окружению и изменяемому состоянию одновременно."
  putStrLn "Подробнее о монадных трансформерах мы поговорим в следующем примере."

-- Главная функция
main :: IO ()
main = do
  putStrLn "State и Reader монады в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "\nКлючевые моменты о State и Reader монадах:"
  putStrLn "1. State представляет вычисление с изменяемым состоянием"
  putStrLn "2. Reader представляет вычисление с доступом к окружению"
  putStrLn "3. State позволяет читать и изменять состояние с помощью функций get, put и modify"
  putStrLn "4. Reader позволяет читать окружение с помощью функций ask, asks и local"
  putStrLn "5. Обе монады позволяют писать код в монадическом стиле с использованием do-нотации"
