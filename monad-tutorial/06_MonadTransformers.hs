{-
  Монадные трансформеры в Haskell
  
  В этом файле мы рассмотрим монадные трансформеры и их применение.
-}

module Main where

import Control.Monad (liftM, ap)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (StateT, runStateT, evalStateT, execStateT, get, put, modify)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Data.Monoid (Sum(..))

-- Монадные трансформеры
-- Монадные трансформеры позволяют комбинировать монады
-- Например, StateT s m a - это монада State, обернутая вокруг монады m

-- Пример: StateT + IO
-- Тип StateT s IO a представляет вычисление, которое может изменять состояние и выполнять операции ввода-вывода
type GameStateIO = StateT GameState IO

data GameState = GameState
  { score :: Int
  , level :: Int
  , lives :: Int
  } deriving (Show)

initialState :: GameState
initialState = GameState { score = 0, level = 1, lives = 3 }

-- Функции для изменения состояния игры
addScore :: Int -> GameStateIO ()
addScore points = modify $ \s -> s { score = score s + points }

levelUp :: GameStateIO ()
levelUp = modify $ \s -> s { level = level s + 1 }

loseLife :: GameStateIO ()
loseLife = modify $ \s -> s { lives = lives s - 1 }

-- Функция для вывода текущего состояния игры
printGameState :: GameStateIO ()
printGameState = do
  state <- get
  lift $ putStrLn $ "Score: " ++ show (score state) ++
                    ", Level: " ++ show (level state) ++
                    ", Lives: " ++ show (lives state)

-- Игровая логика
playGame :: GameStateIO ()
playGame = do
  lift $ putStrLn "Starting the game..."
  printGameState
  
  lift $ putStrLn "Collecting points..."
  addScore 100
  printGameState
  
  lift $ putStrLn "Level up!"
  levelUp
  printGameState
  
  lift $ putStrLn "Collecting more points..."
  addScore 200
  printGameState
  
  lift $ putStrLn "Oops, lost a life!"
  loseLife
  printGameState
  
  lift $ putStrLn "Game over!"

-- Пример: ReaderT + StateT + IO
-- Тип ReaderT r (StateT s IO) a представляет вычисление, которое имеет доступ к окружению,
-- может изменять состояние и выполнять операции ввода-вывода
type AppMonad r s = ReaderT r (StateT s IO)

data AppConfig = AppConfig
  { appName :: String
  , appVersion :: String
  , debugMode :: Bool
  } deriving (Show)

data AppState = AppState
  { counter :: Int
  , lastOperation :: String
  } deriving (Show)

initialAppState :: AppState
initialAppState = AppState { counter = 0, lastOperation = "none" }

-- Функции для работы с AppMonad
getAppName :: AppMonad AppConfig AppState String
getAppName = do
  config <- ask
  return $ appName config

incrementCounter :: AppMonad AppConfig AppState ()
incrementCounter = do
  lift $ modify $ \s -> s { counter = counter s + 1, lastOperation = "increment" }

printAppState :: AppMonad AppConfig AppState ()
printAppState = do
  config <- ask
  state <- lift get
  lift $ lift $ putStrLn $ "App: " ++ appName config ++
                           ", Counter: " ++ show (counter state) ++
                           ", Last operation: " ++ lastOperation state

runApp :: AppMonad AppConfig AppState ()
runApp = do
  name <- getAppName
  lift $ lift $ putStrLn $ "Running app: " ++ name
  
  printAppState
  
  lift $ lift $ putStrLn "Incrementing counter..."
  incrementCounter
  printAppState
  
  lift $ lift $ putStrLn "Incrementing counter again..."
  incrementCounter
  printAppState

-- Пример: MaybeT + IO
-- Тип MaybeT IO a представляет вычисление, которое может завершиться неудачей и выполнять операции ввода-вывода
type MaybeIO = MaybeT IO

-- Функция, которая может завершиться неудачей
safeDivide :: Double -> Double -> MaybeIO Double
safeDivide _ 0 = do
  lift $ putStrLn "Error: Division by zero"
  MaybeT $ return Nothing
safeDivide x y = do
  lift $ putStrLn $ "Dividing " ++ show x ++ " by " ++ show y
  MaybeT $ return $ Just (x / y)

-- Цепочка вычислений с MaybeT
complexCalculation :: Double -> Double -> Double -> MaybeIO Double
complexCalculation x y z = do
  lift $ putStrLn "Starting complex calculation..."
  
  result1 <- safeDivide x y
  lift $ putStrLn $ "First result: " ++ show result1
  
  result2 <- safeDivide result1 z
  lift $ putStrLn $ "Second result: " ++ show result2
  
  MaybeT $ return $ Just (result2 * 2)

-- Пример: ExceptT + IO
-- Тип ExceptT e IO a представляет вычисление, которое может завершиться ошибкой с информацией о причине
-- и выполнять операции ввода-вывода
type ExceptIO = ExceptT String IO

-- Функция, которая может завершиться ошибкой
safeDivideE :: Double -> Double -> ExceptIO Double
safeDivideE _ 0 = do
  lift $ putStrLn "Error: Division by zero"
  throwE "Division by zero"
safeDivideE x y = do
  lift $ putStrLn $ "Dividing " ++ show x ++ " by " ++ show y
  return (x / y)

-- Обработка ошибок с ExceptT
handleDivisionError :: ExceptIO Double -> ExceptIO Double
handleDivisionError action = catchE action $ \err -> do
  lift $ putStrLn $ "Caught error: " ++ err
  return 0

-- Цепочка вычислений с ExceptT
complexCalculationE :: Double -> Double -> Double -> ExceptIO Double
complexCalculationE x y z = do
  lift $ putStrLn "Starting complex calculation with error handling..."
  
  result1 <- handleDivisionError $ safeDivideE x y
  lift $ putStrLn $ "First result: " ++ show result1
  
  result2 <- handleDivisionError $ safeDivideE result1 z
  lift $ putStrLn $ "Second result: " ++ show result2
  
  return (result2 * 2)

-- Пример: WriterT + IO
-- Тип WriterT w IO a представляет вычисление, которое может производить дополнительный вывод
-- и выполнять операции ввода-вывода
type LoggingIO = WriterT [String] IO

-- Функция для логирования
logMessage :: String -> LoggingIO ()
logMessage msg = do
  lift $ putStrLn $ "LOG: " ++ msg
  tell [msg]

-- Функция с логированием
processWithLogging :: Int -> LoggingIO Int
processWithLogging n = do
  logMessage $ "Processing number: " ++ show n
  
  let result = n * 2
  logMessage $ "Result: " ++ show result
  
  return result

-- Цепочка вычислений с WriterT
processListWithLogging :: [Int] -> LoggingIO [Int]
processListWithLogging [] = do
  logMessage "Processing empty list"
  return []
processListWithLogging (x:xs) = do
  logMessage $ "Processing list with head: " ++ show x
  
  y <- processWithLogging x
  ys <- processListWithLogging xs
  
  logMessage "Finished processing list"
  return (y:ys)

-- Пример 1: Использование StateT + IO
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Использование StateT + IO"
  
  putStrLn "Запуск игры с StateT + IO:"
  _ <- runStateT playGame initialState
  
  putStrLn ""

-- Пример 2: Использование ReaderT + StateT + IO
example2 :: IO ()
example2 = do
  putStrLn "Пример 2: Использование ReaderT + StateT + IO"
  
  let config = AppConfig { appName = "MyApp", appVersion = "1.0.0", debugMode = True }
  
  putStrLn "Запуск приложения с ReaderT + StateT + IO:"
  _ <- runStateT (runReaderT runApp config) initialAppState
  
  putStrLn ""

-- Пример 3: Использование MaybeT + IO
example3 :: IO ()
example3 = do
  putStrLn "Пример 3: Использование MaybeT + IO"
  
  putStrLn "Успешное вычисление с MaybeT + IO:"
  result1 <- runMaybeT $ complexCalculation 10 2 5
  putStrLn $ "Результат: " ++ show result1
  
  putStrLn "\nНеудачное вычисление с MaybeT + IO:"
  result2 <- runMaybeT $ complexCalculation 10 0 5
  putStrLn $ "Результат: " ++ show result2
  
  putStrLn ""

-- Пример 4: Использование ExceptT + IO
example4 :: IO ()
example4 = do
  putStrLn "Пример 4: Использование ExceptT + IO"
  
  putStrLn "Успешное вычисление с ExceptT + IO:"
  result1 <- runExceptT $ complexCalculationE 10 2 5
  case result1 of
    Left err -> putStrLn $ "Ошибка: " ++ err
    Right val -> putStrLn $ "Результат: " ++ show val
  
  putStrLn "\nНеудачное вычисление с ExceptT + IO:"
  result2 <- runExceptT $ complexCalculationE 10 0 5
  case result2 of
    Left err -> putStrLn $ "Ошибка: " ++ err
    Right val -> putStrLn $ "Результат: " ++ show val
  
  putStrLn ""

-- Пример 5: Использование WriterT + IO
example5 :: IO ()
example5 = do
  putStrLn "Пример 5: Использование WriterT + IO"
  
  putStrLn "Обработка списка с WriterT + IO:"
  (result, logs) <- runWriterT $ processListWithLogging [1, 2, 3]
  
  putStrLn $ "Результат: " ++ show result
  putStrLn "Логи:"
  mapM_ putStrLn logs
  
  putStrLn ""

-- Пример 6: Стек монадных трансформеров
example6 :: IO ()
example6 = do
  putStrLn "Пример 6: Стек монадных трансформеров"
  
  putStrLn "При использовании нескольких монадных трансформеров важно понимать их порядок:"
  putStrLn "- Внешний трансформер определяет основное поведение"
  putStrLn "- Внутренние трансформеры доступны через lift"
  putStrLn "- Порядок трансформеров влияет на способ доступа к их функциональности"
  
  putStrLn "\nПримеры стеков монадных трансформеров:"
  putStrLn "- StateT s (ReaderT r IO) - состояние снаружи, окружение внутри"
  putStrLn "- ReaderT r (StateT s IO) - окружение снаружи, состояние внутри"
  putStrLn "- ExceptT e (StateT s IO) - обработка ошибок снаружи, состояние внутри"
  putStrLn "- StateT s (ExceptT e IO) - состояние снаружи, обработка ошибок внутри"
  
  putStrLn "\nФункция lift используется для поднятия операций из внутренней монады во внешнюю:"
  putStrLn "- lift :: (MonadTrans t, Monad m) => m a -> t m a"
  putStrLn "- Для доступа к монаде, находящейся глубже одного уровня, нужно использовать lift несколько раз"
  putStrLn "  Например: lift $ lift $ putStrLn \"Hello\""

-- Главная функция
main :: IO ()
main = do
  putStrLn "Монадные трансформеры в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  example6
  
  putStrLn "Ключевые моменты о монадных трансформерах:"
  putStrLn "1. Монадные трансформеры позволяют комбинировать функциональность разных монад"
  putStrLn "2. Каждый трансформер добавляет свою функциональность к базовой монаде"
  putStrLn "3. Функция lift используется для доступа к операциям внутренней монады"
  putStrLn "4. Порядок трансформеров в стеке имеет значение"
  putStrLn "5. Основные монадные трансформеры: StateT, ReaderT, WriterT, MaybeT, ExceptT"
