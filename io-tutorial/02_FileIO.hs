{-
  Работа с файлами в Haskell
  
  В этом файле мы рассмотрим операции ввода-вывода для работы с файлами,
  включая чтение и запись файлов, обработку текстовых данных и управление файловыми дескрипторами.
-}

module Main where

import System.IO
import Control.Exception (bracket, try)
import Control.Monad (when)
import Data.List (sort, isInfixOf)
-- Эти модули требуют установки пакета text, поэтому мы их закомментировали
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO

-- Работа с файлами в Haskell осуществляется через монаду IO
-- и функции из модуля System.IO.

-- Пример 1: Базовые операции с файлами

-- Чтение всего содержимого файла в строку
readEntireFile :: FilePath -> IO String
readEntireFile path = readFile path

-- Запись строки в файл (перезаписывает существующий файл)
writeToFile :: FilePath -> String -> IO ()
writeToFile path content = writeFile path content

-- Добавление строки в конец файла
appendToFile :: FilePath -> String -> IO ()
appendToFile path content = appendFile path content

-- Пример 2: Работа с файловыми дескрипторами

-- Открытие файла и получение дескриптора
openFileExample :: FilePath -> IO Handle
openFileExample path = openFile path ReadMode

-- Чтение содержимого файла через дескриптор
readFromHandle :: Handle -> IO String
readFromHandle handle = hGetContents handle

-- Запись в файл через дескриптор
writeToHandle :: Handle -> String -> IO ()
writeToHandle handle content = hPutStr handle content

-- Закрытие файлового дескриптора
closeFileExample :: Handle -> IO ()
closeFileExample = hClose

-- Пример 3: Безопасная работа с файлами с помощью bracket

-- Безопасное чтение файла с автоматическим закрытием дескриптора
safeReadFile :: FilePath -> IO String
safeReadFile path = do
  content <- readFile path  -- readFile автоматически закрывает файл после чтения
  return content

-- Безопасная запись в файл с автоматическим закрытием дескриптора
safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile path content = bracket
  (openFile path WriteMode) -- открытие файла
  hClose                    -- закрытие файла (выполняется всегда)
  (\handle -> hPutStr handle content) -- запись содержимого

-- Пример 4: Построчная обработка файлов

-- Чтение файла построчно и обработка каждой строки
processFileByLines :: FilePath -> (String -> String) -> IO [String]
processFileByLines path processLine = do
  content <- readFile path
  let lines' = lines content
  let processedLines = map processLine lines'
  return processedLines

-- Запись списка строк в файл
writeLines :: FilePath -> [String] -> IO ()
writeLines path lines' = writeFile path (unlines lines')

-- Фильтрация строк файла по предикату
filterFileLines :: FilePath -> (String -> Bool) -> IO [String]
filterFileLines path predicate = do
  content <- readFile path
  let lines' = lines content
  let filteredLines = filter predicate lines'
  return filteredLines

-- Пример 5: Эффективная работа с текстовыми файлами

-- Чтение файла в строку (стандартная функция)
readFileAsString :: FilePath -> IO String
readFileAsString = readFile

-- Запись строки в файл
writeStringToFile :: FilePath -> String -> IO ()
writeStringToFile = writeFile

-- Обработка текстового файла с помощью функций для работы со строками
processStringFile :: FilePath -> (String -> String) -> IO String
processStringFile path processString = do
  content <- readFile path
  let processed = processString content
  return processed

-- Пример 6: Работа с бинарными файлами

-- Установка бинарного режима для файлового дескриптора
setBinaryMode :: Handle -> IO ()
setBinaryMode handle = hSetBinaryMode handle True

-- Чтение бинарного файла блоками
readBinaryFileBlocks :: FilePath -> Int -> IO [String]
readBinaryFileBlocks path blockSize = do
  content <- readFile path  -- readFile автоматически закрывает файл после чтения
  let blocks = splitIntoBlocks content blockSize
  return blocks
  where
    splitIntoBlocks :: String -> Int -> [String]
    splitIntoBlocks [] _ = []
    splitIntoBlocks str size =
      let (block, rest) = splitAt size str
      in if null block
         then []
         else block : splitIntoBlocks rest size

-- Пример 7: Дополнительные операции с файлами

-- Проверка существования файла
fileExists :: FilePath -> IO Bool
fileExists path = do
  handle <- tryOpenFile path
  case handle of
    Nothing -> return False
    Just h  -> hClose h >> return True
  where
    tryOpenFile :: FilePath -> IO (Maybe Handle)
    tryOpenFile p = do
      result <- try (openFile p ReadMode) :: IO (Either IOError Handle)
      case result of
        Left _ -> return Nothing
        Right h -> return (Just h)

-- Получение размера файла
getFileSize :: FilePath -> IO Integer
getFileSize path = bracket
  (openFile path ReadMode)
  hClose
  hFileSize

-- Проверка, является ли файл пустым
isFileEmpty :: FilePath -> IO Bool
isFileEmpty path = do
  size <- getFileSize path
  return (size == 0)

-- Копирование файла
copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = do
  content <- readFile src
  writeFile dst content

-- Пример 1: Базовые операции с файлами
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Базовые операции с файлами"
  
  -- Создаем временный файл для примера
  let tempFile = "temp_example1.txt"
  
  -- Запись в файл
  putStrLn "Запись в файл..."
  writeToFile tempFile "Это первая строка.\nЭто вторая строка.\nЭто третья строка."
  
  -- Чтение файла
  putStrLn "Чтение файла:"
  content <- readEntireFile tempFile
  putStrLn content
  
  -- Добавление в файл
  putStrLn "\nДобавление в файл..."
  appendToFile tempFile "\nЭто добавленная строка."
  
  -- Чтение обновленного файла
  putStrLn "Чтение обновленного файла:"
  updatedContent <- readEntireFile tempFile
  putStrLn updatedContent
  
  putStrLn "Временный файл создан: temp_example1.txt"

-- Пример 2: Работа с файловыми дескрипторами
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Работа с файловыми дескрипторами"
  
  -- Используем файл из предыдущего примера
  let tempFile = "temp_example1.txt"
  
  -- Открытие файла
  putStrLn "Открытие файла..."
  handle <- openFileExample tempFile
  
  -- Чтение из дескриптора
  putStrLn "Чтение из дескриптора:"
  content <- readFromHandle handle
  putStrLn content
  
  -- Закрытие дескриптора
  putStrLn "Закрытие дескриптора..."
  closeFileExample handle
  
  -- Открытие файла для записи
  putStrLn "Открытие файла для записи..."
  writeHandle <- openFile tempFile WriteMode
  
  -- Запись через дескриптор
  putStrLn "Запись через дескриптор..."
  writeToHandle writeHandle "Это новое содержимое файла.\nЗаписанное через дескриптор."
  
  -- Закрытие дескриптора
  putStrLn "Закрытие дескриптора..."
  closeFileExample writeHandle
  
  -- Проверка результата
  putStrLn "Проверка результата:"
  newContent <- readEntireFile tempFile
  putStrLn newContent

-- Пример 3: Безопасная работа с файлами
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Безопасная работа с файлами"
  
  -- Создаем новый временный файл
  let tempFile = "temp_example3.txt"
  
  -- Безопасная запись в файл
  putStrLn "Безопасная запись в файл..."
  safeWriteFile tempFile "Это содержимое, записанное безопасным способом.\nДаже если произойдет исключение, файл будет закрыт."
  
  -- Безопасное чтение файла
  putStrLn "Безопасное чтение файла:"
  content <- safeReadFile tempFile
  putStrLn content
  
  putStrLn "Временный файл создан: temp_example3.txt"

-- Пример 4: Построчная обработка файлов
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Построчная обработка файлов"
  
  -- Создаем новый временный файл
  let tempFile = "temp_example4.txt"
  
  -- Запись нескольких строк в файл
  putStrLn "Запись строк в файл..."
  let lines' = ["Первая строка", "Вторая строка", "Третья строка", "Четвертая строка", "Пятая строка"]
  writeLines tempFile lines'
  
  -- Обработка каждой строки (преобразование в верхний регистр)
  putStrLn "Обработка каждой строки (преобразование в верхний регистр):"
  processedLines <- processFileByLines tempFile (map toUpper)
  mapM_ putStrLn processedLines
  
  -- Фильтрация строк
  putStrLn "\nФильтрация строк, содержащих 'Третья':"
  filteredLines <- filterFileLines tempFile (isInfixOf "Третья")
  mapM_ putStrLn filteredLines
  
  -- Сортировка строк
  putStrLn "\nСортировка строк:"
  content <- readEntireFile tempFile
  let sortedLines = sort (lines content)
  mapM_ putStrLn sortedLines
  
  putStrLn "Временный файл создан: temp_example4.txt"
  where
    toUpper c | c >= 'а' && c <= 'я' = toEnum (fromEnum c - fromEnum 'а' + fromEnum 'А')
              | c >= 'a' && c <= 'z' = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
              | otherwise = c

-- Пример 5: Эффективная работа с текстовыми файлами
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Эффективная работа с текстовыми файлами"
  
  -- Создаем новый временный файл
  let tempFile = "temp_example5.txt"
  
  -- Запись текста в файл
  putStrLn "Запись текста в файл..."
  writeStringToFile tempFile "Это текст, записанный с помощью стандартных функций.\nОн работает для всех текстовых данных."
  
  -- Чтение текста из файла
  putStrLn "Чтение текста из файла:"
  text <- readFileAsString tempFile
  putStrLn text
  
  -- Обработка текста
  putStrLn "\nОбработка текста (замена 'текст' на 'ТЕКСТ'):"
  let replaceText str = map (\c -> if c == 'т' || c == 'е' || c == 'к' || c == 'с' then toUpper c else c) str
  processedText <- processStringFile tempFile replaceText
  putStrLn processedText
  
  putStrLn "Временный файл создан: temp_example5.txt"
  where
    toUpper c | c == 'т' = 'Т'
              | c == 'е' = 'Е'
              | c == 'к' = 'К'
              | c == 'с' = 'С'
              | otherwise = c

-- Пример 6: Работа с бинарными файлами
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Работа с бинарными файлами"
  
  -- Создаем новый временный бинарный файл
  let tempFile = "temp_example6.bin"
  
  -- Запись бинарных данных в файл
  putStrLn "Запись бинарных данных в файл..."
  bracket
    (openBinaryFile tempFile WriteMode)
    hClose
    (\handle -> do
      hPutStr handle "Это бинарные данные\0с нулевым символом"
    )
  
  -- Чтение бинарных данных блоками
  putStrLn "Чтение бинарных данных блоками:"
  blocks <- readBinaryFileBlocks tempFile 10
  putStrLn $ "Прочитано " ++ show (length blocks) ++ " блоков:"
  mapM_ (\(i, b) -> putStrLn $ "Блок " ++ show i ++ ": " ++ show b) (zip [1..] blocks)
  
  putStrLn "Временный файл создан: temp_example6.bin"

-- Пример 7: Дополнительные операции с файлами
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Дополнительные операции с файлами"
  
  -- Используем файлы из предыдущих примеров
  let tempFile1 = "temp_example1.txt"
  let tempFile2 = "temp_example7.txt"
  
  -- Проверка существования файла
  putStrLn "Проверка существования файла temp_example1.txt:"
  exists1 <- fileExists tempFile1
  putStrLn $ "Файл существует: " ++ show exists1
  
  putStrLn "Проверка существования несуществующего файла:"
  exists2 <- fileExists "несуществующий_файл.txt"
  putStrLn $ "Файл существует: " ++ show exists2
  
  -- Получение размера файла
  putStrLn "\nПолучение размера файла temp_example1.txt:"
  size <- getFileSize tempFile1
  putStrLn $ "Размер файла: " ++ show size ++ " байт"
  
  -- Копирование файла
  putStrLn "\nКопирование файла temp_example1.txt в temp_example7.txt..."
  copyFile tempFile1 tempFile2
  
  -- Проверка результата копирования
  putStrLn "Проверка результата копирования:"
  content <- readEntireFile tempFile2
  putStrLn content
  
  -- Проверка, является ли файл пустым
  putStrLn "\nПроверка, является ли файл temp_example7.txt пустым:"
  empty <- isFileEmpty tempFile2
  putStrLn $ "Файл пустой: " ++ show empty
  
  putStrLn "Временный файл создан: temp_example7.txt"

-- Функция try импортирована из Control.Exception

-- Главная функция
main :: IO ()
main = do
  putStrLn "Работа с файлами в Haskell\n"
  
  -- Запуск всех примеров
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  
  putStrLn "\nКлючевые моменты о работе с файлами в Haskell:"
  putStrLn "1. Базовые функции для работы с файлами: readFile, writeFile, appendFile"
  putStrLn "2. Для более гибкой работы используются файловые дескрипторы (Handle)"
  putStrLn "3. Функция bracket обеспечивает безопасное закрытие файлов даже при исключениях"
  putStrLn "4. Для эффективной работы с текстом используется модуль Data.Text"
  putStrLn "5. Для бинарных данных используется режим hSetBinaryMode"
  putStrLn "6. Ленивое чтение файлов может привести к проблемам с ресурсами"
  putStrLn "7. Всегда закрывайте файловые дескрипторы после использования"
  
  -- Удаление временных файлов
  putStrLn "\nУдаление временных файлов..."
  mapM_ removeIfExists ["temp_example1.txt", "temp_example3.txt", "temp_example4.txt", "temp_example5.txt", "temp_example6.bin", "temp_example7.txt"]
  putStrLn "Временные файлы удалены."
  where
    removeIfExists path = do
      exists <- fileExists path
      when exists $ do
        bracket
          (openFile path WriteMode)
          hClose
          (\_ -> return ())
        putStrLn $ "Удален файл: " ++ path

-- Функция when импортирована из Control.Monad
