{-
  Продвинутые техники ввода-вывода в Haskell
  
  В этом файле мы рассмотрим продвинутые техники ввода-вывода в Haskell,
  включая асинхронный ввод-вывод, обработку исключений, ресурсы и многое другое.
-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Time
import System.IO
import System.IO.Error
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.Process
import System.Timeout
import System.Random

-- Пример 1: Асинхронный ввод-вывод с помощью Control.Concurrent.Async

-- Выполнение двух задач параллельно и ожидание результатов
runConcurrentlyTasks :: IO a -> IO b -> IO (a, b)
runConcurrentlyTasks task1 task2 = do
  a1 <- async task1
  a2 <- async task2
  (,) <$> wait a1 <*> wait a2

-- Выполнение нескольких задач параллельно и ожидание всех результатов
runConcurrentlyMany :: [IO a] -> IO [a]
runConcurrentlyMany tasks = do
  asyncs <- mapM async tasks
  mapM wait asyncs

-- Выполнение нескольких задач параллельно и получение первого результата
runConcurrentlyFirst :: [IO a] -> IO a
runConcurrentlyFirst tasks = do
  asyncs <- mapM async tasks
  waitAny asyncs >>= \(_, result) -> return result

-- Выполнение задачи с таймаутом
runWithTimeout :: Int -> IO a -> IO (Maybe a)
runWithTimeout micros task = do
  timeout micros task

-- Пример 2: Обработка исключений

-- Безопасное выполнение IO-действия с обработкой исключений
safeIO :: IO a -> IO (Either SomeException a)
safeIO action = try action

-- Безопасное чтение файла с обработкой исключений
safeReadFile :: FilePath -> IO (Either IOError String)
safeReadFile path = tryIOError (readFile path)

-- Безопасное выполнение IO-действия с указанием действия при ошибке
safeIOWithDefault :: a -> IO a -> IO a
safeIOWithDefault defaultValue action = catch action (\(_ :: SomeException) -> return defaultValue)

-- Безопасное выполнение IO-действия с указанием обработчика ошибок
safeIOWithHandler :: (SomeException -> IO a) -> IO a -> IO a
safeIOWithHandler handler action = catch action handler

-- Пример 3: Работа с ресурсами

-- Безопасная работа с файлом с автоматическим закрытием
withFileContents :: FilePath -> (String -> IO a) -> IO a
withFileContents path action = bracket
  (openFile path ReadMode)
  hClose
  (\handle -> hGetContents handle >>= action)

-- Безопасная работа с временным файлом
withTempFileContents :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFileContents template action = do
  tempDir <- getTemporaryDirectory
  bracket
    (openTempFile tempDir template)
    (\(path, handle) -> do
      hClose handle
      removeFile path)
    (uncurry action)

-- Безопасная работа с блокировкой
withLock :: MVar () -> IO a -> IO a
withLock lock action = bracket
  (takeMVar lock)
  (\_ -> putMVar lock ())
  (\_ -> action)

-- Пример 4: Программирование с использованием STM

-- Создание банковского счета с использованием STM
data Account = Account
  { accountId :: Int
  , accountBalance :: TVar Double
  }

-- Создание нового счета
newAccount :: Int -> Double -> IO Account
newAccount id initialBalance = do
  balanceVar <- newTVarIO initialBalance
  return $ Account id balanceVar

-- Получение баланса счета
getBalance :: Account -> IO Double
getBalance account = readTVarIO (accountBalance account)

-- Пополнение счета
deposit :: Account -> Double -> IO ()
deposit account amount = atomically $ do
  balance <- readTVar (accountBalance account)
  writeTVar (accountBalance account) (balance + amount)

-- Снятие со счета
withdraw :: Account -> Double -> IO Bool
withdraw account amount = atomically $ do
  balance <- readTVar (accountBalance account)
  if balance >= amount
    then do
      writeTVar (accountBalance account) (balance - amount)
      return True
    else return False

-- Перевод между счетами
transfer :: Account -> Account -> Double -> IO Bool
transfer from to amount = atomically $ do
  fromBalance <- readTVar (accountBalance from)
  if fromBalance >= amount
    then do
      writeTVar (accountBalance from) (fromBalance - amount)
      toBalance <- readTVar (accountBalance to)
      writeTVar (accountBalance to) (toBalance + amount)
      return True
    else return False

-- Пример 5: Логирование и профилирование

-- Простая функция логирования
logMessage :: String -> IO ()
logMessage msg = do
  time <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
  putStrLn $ formattedTime ++ " - " ++ msg

-- Логирование с уровнями
data LogLevel = DEBUG | INFO | WARNING | ERROR deriving (Eq, Ord, Show)

logWithLevel :: LogLevel -> LogLevel -> String -> IO ()
logWithLevel minLevel level msg =
  when (level >= minLevel) $ do
    time <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
    putStrLn $ formattedTime ++ " [" ++ show level ++ "] - " ++ msg

-- Измерение времени выполнения функции
timeIt :: IO a -> IO (a, NominalDiffTime)
timeIt action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let diff = diffUTCTime end start
  return (result, diff)

-- Логирование времени выполнения функции
logTimeIt :: String -> IO a -> IO a
logTimeIt description action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  let diff = diffUTCTime end start
  logMessage $ description ++ " выполнено за " ++ show diff
  return result

-- Пример 6: Работа с процессами

-- Запуск внешней команды и получение вывода
runCustomCommand :: String -> [String] -> IO String
runCustomCommand cmd args = do
  (_, Just hout, _, ph) <- createProcess (proc cmd args) { std_out = CreatePipe }
  output <- hGetContents hout
  _ <- waitForProcess ph
  return output

-- Запуск внешней команды с перенаправлением ввода и вывода
runCommandWithInput :: String -> [String] -> String -> IO String
runCommandWithInput cmd args input = do
  (Just hin, Just hout, _, ph) <- createProcess (proc cmd args) 
                                    { std_in = CreatePipe, std_out = CreatePipe }
  hPutStr hin input
  hClose hin
  output <- hGetContents hout
  _ <- waitForProcess ph
  return output

-- Запуск внешней команды и обработка кода возврата
runCommandWithExitCode :: String -> [String] -> IO (ExitCode, String, String)
runCommandWithExitCode cmd args = do
  (_, Just hout, Just herr, ph) <- createProcess (proc cmd args) 
                                    { std_out = CreatePipe, std_err = CreatePipe }
  output <- hGetContents hout
  err <- hGetContents herr
  exitCode <- waitForProcess ph
  return (exitCode, output, err)

-- Пример 7: Работа с окружением

-- Получение переменной окружения с значением по умолчанию
getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault name defaultValue = do
  result <- tryIOError (getEnv name)
  case result of
    Left _ -> return defaultValue
    Right value -> return value

-- Безопасное получение всех переменных окружения
getAllEnv :: IO [(String, String)]
getAllEnv = getEnvironment

-- Запуск действия с измененным окружением
withModifiedEnv :: [(String, String)] -> IO a -> IO a
withModifiedEnv modifications action = do
  oldEnv <- getEnvironment
  bracket
    (mapM_ (uncurry setEnv) modifications)
    (\_ -> mapM_ (\(name, _) -> 
                    case lookup name oldEnv of
                      Just value -> setEnv name value
                      Nothing -> unsetEnv name) 
                 modifications)
    (\_ -> action)

-- Пример 1: Асинхронный ввод-вывод с помощью Control.Concurrent.Async
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Асинхронный ввод-вывод с помощью Control.Concurrent.Async"
  
  -- Выполнение двух задач параллельно
  putStrLn "Выполнение двух задач параллельно..."
  (result1, result2) <- runConcurrentlyTasks
    (do
      putStrLn "Задача 1 запущена"
      threadDelay 2000000  -- 2 секунды
      putStrLn "Задача 1 завершена"
      return "Результат задачи 1")
    (do
      putStrLn "Задача 2 запущена"
      threadDelay 1000000  -- 1 секунда
      putStrLn "Задача 2 завершена"
      return "Результат задачи 2")
  
  putStrLn $ "Результаты: " ++ result1 ++ ", " ++ result2
  
  -- Выполнение нескольких задач параллельно
  putStrLn "\nВыполнение нескольких задач параллельно..."
  let tasks = [
        do
          putStrLn "Задача A запущена"
          threadDelay 1500000  -- 1.5 секунды
          putStrLn "Задача A завершена"
          return "Результат задачи A",
        do
          putStrLn "Задача B запущена"
          threadDelay 1000000  -- 1 секунда
          putStrLn "Задача B завершена"
          return "Результат задачи B",
        do
          putStrLn "Задача C запущена"
          threadDelay 2000000  -- 2 секунды
          putStrLn "Задача C завершена"
          return "Результат задачи C"
        ]
  
  results <- runConcurrentlyMany tasks
  putStrLn $ "Все результаты: " ++ show results
  
  -- Выполнение задачи с таймаутом
  putStrLn "\nВыполнение задачи с таймаутом..."
  result <- runWithTimeout 1500000 $ do  -- 1.5 секунды
    putStrLn "Задача с таймаутом запущена"
    threadDelay 2000000  -- 2 секунды
    putStrLn "Задача с таймаутом завершена"
    return "Результат задачи с таймаутом"
  
  case result of
    Nothing -> putStrLn "Задача не успела выполниться за отведенное время"
    Just r -> putStrLn $ "Результат: " ++ r

-- Пример 2: Обработка исключений
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Обработка исключений"
  
  -- Безопасное выполнение IO-действия
  putStrLn "Безопасное выполнение IO-действия..."
  result1 <- safeIO $ do
    putStrLn "Выполняется безопасное действие"
    return (42 :: Int)
  
  case result1 of
    Left err -> putStrLn $ "Произошла ошибка: " ++ show err
    Right value -> putStrLn $ "Результат: " ++ show value
  
  -- Безопасное выполнение IO-действия с ошибкой
  putStrLn "\nБезопасное выполнение IO-действия с ошибкой..."
  result2 <- (safeIO $ do
    putStrLn "Выполняется действие с ошибкой"
    error "Это тестовая ошибка") :: IO (Either SomeException Int)
  
  case result2 of
    Left err -> putStrLn $ "Произошла ошибка: " ++ show err
    Right value -> putStrLn $ "Результат: " ++ show value
  
  -- Безопасное чтение файла
  putStrLn "\nБезопасное чтение файла..."
  let existingFile = "temp_example6.txt"
  
  -- Создание временного файла
  writeFile existingFile "Содержимое временного файла"
  
  result3 <- safeReadFile existingFile
  case result3 of
    Left err -> putStrLn $ "Ошибка при чтении файла: " ++ show err
    Right content -> putStrLn $ "Содержимое файла: " ++ content
  
  -- Безопасное чтение несуществующего файла
  putStrLn "\nБезопасное чтение несуществующего файла..."
  let nonExistingFile = "non_existing_file.txt"
  
  result4 <- safeReadFile nonExistingFile
  case result4 of
    Left err -> putStrLn $ "Ошибка при чтении файла: " ++ show err
    Right content -> putStrLn $ "Содержимое файла: " ++ content
  
  -- Безопасное выполнение IO-действия с значением по умолчанию
  putStrLn "\nБезопасное выполнение IO-действия с значением по умолчанию..."
  result5 <- safeIOWithDefault "Значение по умолчанию" $ do
    error "Это тестовая ошибка"
  
  putStrLn $ "Результат: " ++ result5
  
  -- Безопасное выполнение IO-действия с обработчиком ошибок
  putStrLn "\nБезопасное выполнение IO-действия с обработчиком ошибок..."
  result6 <- safeIOWithHandler (\err -> do
                                  putStrLn $ "Обработка ошибки: " ++ show err
                                  return "Результат обработки ошибки") $ do
    error "Это тестовая ошибка"
  
  putStrLn $ "Результат: " ++ result6

-- Пример 3: Работа с ресурсами
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Работа с ресурсами"
  
  -- Безопасная работа с файлом
  putStrLn "Безопасная работа с файлом..."
  let tempFile = "temp_example6.txt"
  
  -- Создание временного файла
  writeFile tempFile "Содержимое временного файла"
  
  result <- withFileContents tempFile $ \content -> do
    putStrLn $ "Прочитано содержимое файла: " ++ content
    return $ length content
  
  putStrLn $ "Длина содержимого файла: " ++ show result
  
  -- Безопасная работа с временным файлом
  putStrLn "\nБезопасная работа с временным файлом..."
  result2 <- withTempFileContents "example_temp_" $ \path handle -> do
    putStrLn $ "Создан временный файл: " ++ path
    hPutStrLn handle "Содержимое временного файла"
    hSeek handle AbsoluteSeek 0
    content <- hGetContents handle
    putStrLn $ "Прочитано содержимое временного файла: " ++ content
    return $ length content
  
  putStrLn $ "Длина содержимого временного файла: " ++ show result2
  
  -- Безопасная работа с блокировкой
  putStrLn "\nБезопасная работа с блокировкой..."
  lock <- newMVar ()
  
  -- Запуск нескольких потоков, использующих блокировку
  void $ forkIO $ withLock lock $ do
    putStrLn "Поток 1: Получена блокировка"
    threadDelay 2000000  -- 2 секунды
    putStrLn "Поток 1: Освобождена блокировка"
  
  threadDelay 500000  -- 0.5 секунды
  
  void $ forkIO $ withLock lock $ do
    putStrLn "Поток 2: Получена блокировка"
    threadDelay 1000000  -- 1 секунда
    putStrLn "Поток 2: Освобождена блокировка"
  
  threadDelay 4000000  -- 4 секунды, чтобы дождаться завершения потоков

-- Пример 4: Программирование с использованием STM
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Программирование с использованием STM"
  
  -- Создание банковских счетов
  putStrLn "Создание банковских счетов..."
  account1 <- newAccount 1 1000.0
  account2 <- newAccount 2 500.0
  
  -- Получение баланса счетов
  balance1 <- getBalance account1
  balance2 <- getBalance account2
  
  putStrLn $ "Начальный баланс счета 1: " ++ show balance1
  putStrLn $ "Начальный баланс счета 2: " ++ show balance2
  
  -- Пополнение счета
  putStrLn "\nПополнение счета 1 на 200.0..."
  deposit account1 200.0
  
  balance1' <- getBalance account1
  putStrLn $ "Новый баланс счета 1: " ++ show balance1'
  
  -- Снятие со счета
  putStrLn "\nСнятие со счета 2 на 100.0..."
  success1 <- withdraw account2 100.0
  
  balance2' <- getBalance account2
  putStrLn $ "Снятие " ++ (if success1 then "успешно" else "не успешно")
  putStrLn $ "Новый баланс счета 2: " ++ show balance2'
  
  -- Снятие со счета (недостаточно средств)
  putStrLn "\nСнятие со счета 2 на 1000.0 (недостаточно средств)..."
  success2 <- withdraw account2 1000.0
  
  balance2'' <- getBalance account2
  putStrLn $ "Снятие " ++ (if success2 then "успешно" else "не успешно")
  putStrLn $ "Баланс счета 2: " ++ show balance2''
  
  -- Перевод между счетами
  putStrLn "\nПеревод со счета 1 на счет 2 на 300.0..."
  success3 <- transfer account1 account2 300.0
  
  balance1'' <- getBalance account1
  balance2''' <- getBalance account2
  
  putStrLn $ "Перевод " ++ (if success3 then "успешен" else "не успешен")
  putStrLn $ "Новый баланс счета 1: " ++ show balance1''
  putStrLn $ "Новый баланс счета 2: " ++ show balance2'''
  
  -- Перевод между счетами (недостаточно средств)
  putStrLn "\nПеревод со счета 2 на счет 1 на 1000.0 (недостаточно средств)..."
  success4 <- transfer account2 account1 1000.0
  
  balance1''' <- getBalance account1
  balance2'''' <- getBalance account2
  
  putStrLn $ "Перевод " ++ (if success4 then "успешен" else "не успешен")
  putStrLn $ "Баланс счета 1: " ++ show balance1'''
  putStrLn $ "Баланс счета 2: " ++ show balance2''''

-- Пример 5: Логирование и профилирование
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Логирование и профилирование"
  
  -- Простое логирование
  putStrLn "Простое логирование..."
  logMessage "Это тестовое сообщение"
  
  -- Логирование с уровнями
  putStrLn "\nЛогирование с уровнями..."
  let minLevel = INFO
  
  logWithLevel minLevel DEBUG "Это сообщение уровня DEBUG"
  logWithLevel minLevel INFO "Это сообщение уровня INFO"
  logWithLevel minLevel WARNING "Это сообщение уровня WARNING"
  logWithLevel minLevel ERROR "Это сообщение уровня ERROR"
  
  -- Измерение времени выполнения функции
  putStrLn "\nИзмерение времени выполнения функции..."
  (result, time) <- timeIt $ do
    putStrLn "Выполняется функция..."
    threadDelay 1000000  -- 1 секунда
    return 42
  
  putStrLn $ "Результат: " ++ show result
  putStrLn $ "Время выполнения: " ++ show time
  
  -- Логирование времени выполнения функции
  putStrLn "\nЛогирование времени выполнения функции..."
  result' <- logTimeIt "Тестовая функция" $ do
    putStrLn "Выполняется функция..."
    threadDelay 1000000  -- 1 секунда
    return 42
  
  putStrLn $ "Результат: " ++ show result'

-- Пример 6: Работа с процессами
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Работа с процессами"
  
  -- Запуск внешней команды и получение вывода
  putStrLn "Запуск внешней команды и получение вывода..."
  output <- runCustomCommand "echo" ["Hello, World!"]
  
  putStrLn $ "Вывод команды: " ++ output
  
  -- Запуск внешней команды с перенаправлением ввода
  putStrLn "\nЗапуск внешней команды с перенаправлением ввода..."
  output2 <- runCommandWithInput "cat" [] "Hello from stdin!"
  
  putStrLn $ "Вывод команды: " ++ output2
  
  -- Запуск внешней команды и обработка кода возврата
  putStrLn "\nЗапуск внешней команды и обработка кода возврата..."
  (exitCode, output3, err) <- runCommandWithExitCode "ls" ["-la"]
  
  putStrLn $ "Код возврата: " ++ show exitCode
  putStrLn $ "Вывод команды: " ++ take 100 output3 ++ "..."
  putStrLn $ "Ошибки: " ++ err

-- Пример 7: Работа с окружением
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Работа с окружением"
  
  -- Получение переменной окружения с значением по умолчанию
  putStrLn "Получение переменной окружения с значением по умолчанию..."
  value <- getEnvWithDefault "EXAMPLE_VAR" "Значение по умолчанию"
  
  putStrLn $ "Значение переменной EXAMPLE_VAR: " ++ value
  
  -- Безопасное получение всех переменных окружения
  putStrLn "\nБезопасное получение всех переменных окружения..."
  env <- getAllEnv
  
  putStrLn $ "Количество переменных окружения: " ++ show (length env)
  putStrLn "Некоторые переменные окружения:"
  forM_ (take 5 env) $ \(name, value) ->
    putStrLn $ name ++ "=" ++ value
  
  -- Запуск действия с измененным окружением
  putStrLn "\nЗапуск действия с измененным окружением..."
  withModifiedEnv [("EXAMPLE_VAR", "Новое значение")] $ do
    value' <- getEnv "EXAMPLE_VAR"
    putStrLn $ "Значение переменной EXAMPLE_VAR внутри действия: " ++ value'
  
  value'' <- getEnvWithDefault "EXAMPLE_VAR" "Значение по умолчанию"
  putStrLn $ "Значение переменной EXAMPLE_VAR после действия: " ++ value''

-- Главная функция
main :: IO ()
main = do
  putStrLn "Продвинутые техники ввода-вывода в Haskell\n"
  
  -- Запуск всех примеров
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  
  putStrLn "\nКлючевые моменты о продвинутых техниках ввода-вывода в Haskell:"
  putStrLn "1. Асинхронный ввод-вывод позволяет выполнять несколько задач параллельно"
  putStrLn "2. Обработка исключений позволяет безопасно выполнять IO-действия"
  putStrLn "3. Работа с ресурсами позволяет безопасно использовать ресурсы, такие как файлы и сокеты"
  putStrLn "4. STM (Software Transactional Memory) позволяет безопасно работать с разделяемыми данными"
  putStrLn "5. Логирование и профилирование позволяют отслеживать выполнение программы"
  putStrLn "6. Работа с процессами позволяет запускать внешние команды и взаимодействовать с ними"
  putStrLn "7. Работа с окружением позволяет получать и изменять переменные окружения"
