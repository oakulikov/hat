{-
  Работа с директориями в Haskell
  
  В этом файле мы рассмотрим операции ввода-вывода для работы с директориями,
  включая создание, удаление, перемещение и получение информации о директориях.
-}

module Main where

import System.Directory hiding (findFiles)
import System.Directory (Permissions(..), getPermissions, setPermissions, setOwnerExecutable)
import System.IO.Temp (withTempFile, withTempDirectory)
import System.FilePath
import System.IO (Handle, hClose, hPutStrLn)
import Control.Monad (when, forM_, forM)
import Control.Exception (bracket, try)
import Data.List (sort)
import Data.Time.Clock (UTCTime)

-- Работа с директориями в Haskell осуществляется через функции из модуля System.Directory.

-- Пример 1: Базовые операции с директориями

-- Получение текущей директории
getCurrentDirectoryExample :: IO FilePath
getCurrentDirectoryExample = getCurrentDirectory

-- Изменение текущей директории
setCurrentDirectoryExample :: FilePath -> IO ()
setCurrentDirectoryExample path = setCurrentDirectory path

-- Создание директории
createDirectoryExample :: FilePath -> IO ()
createDirectoryExample path = createDirectory path

-- Создание директории с родительскими директориями
createDirectoryIfMissingExample :: FilePath -> IO ()
createDirectoryIfMissingExample path = createDirectoryIfMissing True path

-- Удаление директории
removeDirectoryExample :: FilePath -> IO ()
removeDirectoryExample path = removeDirectory path

-- Удаление директории рекурсивно
removeDirectoryRecursiveExample :: FilePath -> IO ()
removeDirectoryRecursiveExample path = removeDirectoryRecursive path

-- Пример 2: Получение информации о директориях

-- Проверка существования директории
doesDirectoryExistExample :: FilePath -> IO Bool
doesDirectoryExistExample path = doesDirectoryExist path

-- Проверка существования файла
doesFileExistExample :: FilePath -> IO Bool
doesFileExistExample path = doesFileExist path

-- Получение содержимого директории
getDirectoryContentsExample :: FilePath -> IO [FilePath]
getDirectoryContentsExample path = getDirectoryContents path

-- Получение содержимого директории (только видимые файлы)
listDirectoryExample :: FilePath -> IO [FilePath]
listDirectoryExample path = listDirectory path

-- Пример 3: Работа с путями к файлам

-- Получение абсолютного пути
makeAbsolutePathExample :: FilePath -> IO FilePath
makeAbsolutePathExample path = makeAbsolute path

-- Получение канонического пути
canonicalizePathExample :: FilePath -> IO FilePath
canonicalizePathExample path = canonicalizePath path

-- Получение домашней директории пользователя
getHomeDirectoryExample :: IO FilePath
getHomeDirectoryExample = getHomeDirectory

-- Получение временной директории
getTempDirectoryExample :: IO FilePath
getTempDirectoryExample = getTemporaryDirectory

-- Пример 4: Работа с атрибутами файлов и директорий

-- Получение времени модификации файла
getModificationTimeExample :: FilePath -> IO UTCTime
getModificationTimeExample path = getModificationTime path

-- Получение размера файла
getFileSizeExample :: FilePath -> IO Integer
getFileSizeExample path = getFileSize path

-- Проверка прав доступа к файлу
getPermissionsExample :: FilePath -> IO Permissions
getPermissionsExample path = getPermissions path

-- Изменение прав доступа к файлу
setPermissionsExample :: FilePath -> Permissions -> IO ()
setPermissionsExample path perms = setPermissions path perms

-- Пример 5: Рекурсивная обработка директорий

-- Рекурсивный обход директории
walkDirectory :: FilePath -> (FilePath -> IO ()) -> IO ()
walkDirectory dir action = do
  action dir
  isDir <- doesDirectoryExist dir
  when isDir $ do
    contents <- listDirectory dir
    forM_ contents $ \item -> do
      let path = dir </> item
      walkDirectory path action

-- Поиск файлов по предикату
findFiles :: FilePath -> (FilePath -> IO Bool) -> IO [FilePath]
findFiles dir predicate = do
  isDir <- doesDirectoryExist dir
  if isDir
    then do
      contents <- listDirectory dir
      paths <- forM contents $ \item -> do
        let path = dir </> item
        isDir' <- doesDirectoryExist path
        if isDir'
          then findFiles path predicate
          else do
            matches <- predicate path
            return [path | matches]
      return (concat paths)
    else do
      matches <- predicate dir
      return [dir | matches]

-- Пример 6: Копирование и перемещение файлов и директорий

-- Копирование файла
copyFileExample :: FilePath -> FilePath -> IO ()
copyFileExample src dst = copyFile src dst

-- Перемещение файла или директории
renameFileExample :: FilePath -> FilePath -> IO ()
renameFileExample src dst = renameFile src dst

-- Копирование директории рекурсивно
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
  createDirectoryIfMissing True dst
  contents <- listDirectory src
  forM_ contents $ \item -> do
    let srcPath = src </> item
    let dstPath = dst </> item
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyDirectoryRecursive srcPath dstPath
      else copyFile srcPath dstPath

-- Пример 7: Работа с временными файлами и директориями

-- Создание временного файла
withTempFileExample :: (FilePath -> Handle -> IO a) -> IO a
withTempFileExample action = do
  tempDir <- getTemporaryDirectory
  withTempFile tempDir "example.tmp" action

-- Создание временной директории
withTempDirectoryExample :: (FilePath -> IO a) -> IO a
withTempDirectoryExample action = do
  tempDir <- getTemporaryDirectory
  withTempDirectory tempDir "example-dir" action

-- Пример 1: Базовые операции с директориями
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Базовые операции с директориями"
  
  -- Получение текущей директории
  putStrLn "Получение текущей директории:"
  currentDir <- getCurrentDirectoryExample
  putStrLn currentDir
  
  -- Создание временной директории для примеров
  let tempDir = "temp_dir_example"
  
  -- Проверка существования директории
  exists <- doesDirectoryExistExample tempDir
  when exists $ do
    putStrLn "Удаление существующей директории..."
    removeDirectoryRecursiveExample tempDir
  
  -- Создание директории
  putStrLn "\nСоздание директории..."
  createDirectoryExample tempDir
  
  -- Создание вложенной директории
  putStrLn "Создание вложенной директории..."
  let nestedDir = tempDir </> "nested" </> "dir"
  createDirectoryIfMissingExample nestedDir
  
  -- Проверка результата
  putStrLn "Проверка результата:"
  exists1 <- doesDirectoryExistExample tempDir
  putStrLn $ "Директория " ++ tempDir ++ " существует: " ++ show exists1
  
  exists2 <- doesDirectoryExistExample nestedDir
  putStrLn $ "Директория " ++ nestedDir ++ " существует: " ++ show exists2
  
  putStrLn $ "Временная директория создана: " ++ tempDir

-- Пример 2: Получение информации о директориях
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Получение информации о директориях"
  
  -- Используем директорию из предыдущего примера
  let tempDir = "temp_dir_example"
  
  -- Создание нескольких файлов для примера
  putStrLn "Создание файлов для примера..."
  writeFile (tempDir </> "file1.txt") "Содержимое файла 1"
  writeFile (tempDir </> "file2.txt") "Содержимое файла 2"
  writeFile (tempDir </> "file3.dat") "Содержимое файла 3"
  
  -- Получение содержимого директории
  putStrLn "\nПолучение содержимого директории (включая скрытые файлы):"
  contents1 <- getDirectoryContentsExample tempDir
  mapM_ putStrLn contents1
  
  -- Получение содержимого директории (только видимые файлы)
  putStrLn "\nПолучение содержимого директории (только видимые файлы):"
  contents2 <- listDirectoryExample tempDir
  mapM_ putStrLn contents2
  
  -- Проверка существования файла
  putStrLn "\nПроверка существования файла:"
  exists1 <- doesFileExistExample (tempDir </> "file1.txt")
  putStrLn $ "Файл file1.txt существует: " ++ show exists1
  
  exists2 <- doesFileExistExample (tempDir </> "nonexistent.txt")
  putStrLn $ "Файл nonexistent.txt существует: " ++ show exists2

-- Пример 3: Работа с путями к файлам
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Работа с путями к файлам"
  
  -- Получение абсолютного пути
  putStrLn "Получение абсолютного пути:"
  absPath <- makeAbsolutePathExample "temp_dir_example/file1.txt"
  putStrLn absPath
  
  -- Получение канонического пути
  putStrLn "\nПолучение канонического пути:"
  canonPath <- canonicalizePathExample "temp_dir_example/./nested/../file1.txt"
  putStrLn canonPath
  
  -- Получение домашней директории пользователя
  putStrLn "\nПолучение домашней директории пользователя:"
  homeDir <- getHomeDirectoryExample
  putStrLn homeDir
  
  -- Получение временной директории
  putStrLn "\nПолучение временной директории:"
  tempDir <- getTempDirectoryExample
  putStrLn tempDir
  
  -- Работа с путями (без ввода-вывода)
  putStrLn "\nРабота с путями (без ввода-вывода):"
  let path1 = "dir/file.txt"
  putStrLn $ "Исходный путь: " ++ path1
  putStrLn $ "Директория: " ++ takeDirectory path1
  putStrLn $ "Имя файла: " ++ takeFileName path1
  putStrLn $ "Имя файла без расширения: " ++ takeBaseName path1
  putStrLn $ "Расширение: " ++ takeExtension path1
  
  let path2 = "dir1" </> "dir2" </> "file.txt"
  putStrLn $ "\nОбъединение путей: " ++ path2

-- Пример 4: Работа с атрибутами файлов и директорий
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Работа с атрибутами файлов и директорий"
  
  -- Используем файл из предыдущего примера
  let filePath = "temp_dir_example/file1.txt"
  
  -- Получение времени модификации файла
  putStrLn "Получение времени модификации файла:"
  modTime <- getModificationTimeExample filePath
  putStrLn $ show modTime
  
  -- Получение размера файла
  putStrLn "\nПолучение размера файла:"
  size <- getFileSizeExample filePath
  putStrLn $ show size ++ " байт"
  
  -- Получение прав доступа к файлу
  putStrLn "\nПолучение прав доступа к файлу:"
  perms <- getPermissionsExample filePath
  putStrLn $ "Чтение: " ++ show (readable perms)
  putStrLn $ "Запись: " ++ show (writable perms)
  putStrLn $ "Выполнение: " ++ show (executable perms)
  putStrLn $ "Поиск: " ++ show (searchable perms)
  
  -- Изменение прав доступа к файлу
  putStrLn "\nИзменение прав доступа к файлу:"
  let newPerms = setOwnerExecutable True perms
  setPermissionsExample filePath newPerms
  
  -- Проверка результата
  putStrLn "Проверка результата:"
  perms' <- getPermissionsExample filePath
  putStrLn $ "Выполнение: " ++ show (executable perms')

-- Пример 5: Рекурсивная обработка директорий
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Рекурсивная обработка директорий"
  
  -- Используем директорию из предыдущего примера
  let tempDir = "temp_dir_example"
  
  -- Рекурсивный обход директории
  putStrLn "Рекурсивный обход директории:"
  walkDirectory tempDir $ \path -> do
    isDir <- doesDirectoryExist path
    let prefix = if isDir then "Директория: " else "Файл: "
    putStrLn $ prefix ++ path
  
  -- Поиск файлов по предикату (все файлы с расширением .txt)
  putStrLn "\nПоиск файлов с расширением .txt:"
  txtFiles <- findFiles tempDir $ \path -> do
    isFile <- doesFileExist path
    return (isFile && takeExtension path == ".txt")
  mapM_ putStrLn txtFiles

-- Пример 6: Копирование и перемещение файлов и директорий
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Копирование и перемещение файлов и директорий"
  
  -- Используем директорию из предыдущего примера
  let tempDir = "temp_dir_example"
  let copyDir = "temp_dir_copy"
  
  -- Удаление директории копии, если она существует
  exists <- doesDirectoryExistExample copyDir
  when exists $ do
    putStrLn "Удаление существующей директории копии..."
    removeDirectoryRecursiveExample copyDir
  
  -- Копирование директории рекурсивно
  putStrLn "Копирование директории рекурсивно..."
  copyDirectoryRecursive tempDir copyDir
  
  -- Проверка результата
  putStrLn "Проверка результата:"
  contents <- listDirectoryExample copyDir
  mapM_ putStrLn contents
  
  -- Перемещение файла
  putStrLn "\nПеремещение файла..."
  let srcFile = copyDir </> "file1.txt"
  let dstFile = copyDir </> "moved_file1.txt"
  renameFileExample srcFile dstFile
  
  -- Проверка результата
  putStrLn "Проверка результата:"
  contents' <- listDirectoryExample copyDir
  mapM_ putStrLn contents'
  
  putStrLn $ "Временная директория копии создана: " ++ copyDir

-- Пример 7: Работа с временными файлами и директориями
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Работа с временными файлами и директориями"
  
  -- Создание временного файла
  putStrLn "Создание временного файла:"
  result1 <- withTempFileExample $ \path handle -> do
    putStrLn $ "Временный файл создан: " ++ path
    hPutStrLn handle "Содержимое временного файла"
    hClose handle
    
    -- Чтение содержимого временного файла
    content <- readFile path
    return content
  
  putStrLn $ "Содержимое временного файла: " ++ result1
  
  -- Создание временной директории
  putStrLn "\nСоздание временной директории:"
  result2 <- withTempDirectoryExample $ \path -> do
    putStrLn $ "Временная директория создана: " ++ path
    
    -- Создание файла во временной директории
    let filePath = path </> "temp_file.txt"
    writeFile filePath "Содержимое файла во временной директории"
    
    -- Чтение содержимого файла
    readFile filePath
  
  putStrLn $ "Содержимое файла во временной директории: " ++ result2

-- Главная функция
main :: IO ()
main = do
  putStrLn "Работа с директориями в Haskell\n"
  
  -- Запуск всех примеров
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  
  putStrLn "\nКлючевые моменты о работе с директориями в Haskell:"
  putStrLn "1. Модуль System.Directory предоставляет функции для работы с директориями"
  putStrLn "2. Модуль System.FilePath предоставляет функции для работы с путями к файлам"
  putStrLn "3. Функции createDirectory и removeDirectory создают и удаляют директории"
  putStrLn "4. Функции getDirectoryContents и listDirectory получают содержимое директории"
  putStrLn "5. Функции doesDirectoryExist и doesFileExist проверяют существование директорий и файлов"
  putStrLn "6. Функции copyFile и renameFile копируют и перемещают файлы"
  putStrLn "7. Функции withTempFile и withTempDirectory создают временные файлы и директории"
  
  -- Удаление временных директорий
  putStrLn "\nУдаление временных директорий..."
  removeIfExists "temp_dir_example"
  removeIfExists "temp_dir_copy"
  putStrLn "Временные директории удалены."
  where
    removeIfExists path = do
      exists <- doesDirectoryExist path
      when exists $ do
        removeDirectoryRecursive path
        putStrLn $ "Удалена директория: " ++ path
