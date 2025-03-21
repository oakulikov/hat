{-
  Работа с бинарными данными в Haskell
  
  В этом файле мы рассмотрим операции ввода-вывода для работы с бинарными данными,
  включая чтение и запись бинарных файлов, сериализацию и десериализацию данных.
-}

module Main where

import System.IO
import System.Directory (doesFileExist, removeFile)
import Control.Exception (bracket, try)
import Control.Monad (when, forM_, replicateM)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Int
import Data.Bits
import Data.Char (chr, ord)

-- Работа с бинарными данными в Haskell осуществляется через модули
-- Data.Binary, Data.ByteString и другие.

-- Пример 1: Базовые операции с бинарными файлами

-- Чтение бинарного файла в ByteString
readBinaryFile :: FilePath -> IO BS.ByteString
readBinaryFile path = BS.readFile path

-- Запись ByteString в бинарный файл
writeBinaryFile :: FilePath -> BS.ByteString -> IO ()
writeBinaryFile path content = BS.writeFile path content

-- Чтение бинарного файла в Lazy ByteString
readLazyBinaryFile :: FilePath -> IO BL.ByteString
readLazyBinaryFile path = BL.readFile path

-- Запись Lazy ByteString в бинарный файл
writeLazyBinaryFile :: FilePath -> BL.ByteString -> IO ()
writeLazyBinaryFile path content = BL.writeFile path content

-- Пример 2: Работа с бинарными данными через Handle

-- Открытие бинарного файла для чтения
openBinaryFileForReading :: FilePath -> IO Handle
openBinaryFileForReading path = do
  handle <- openBinaryFile path ReadMode
  return handle

-- Открытие бинарного файла для записи
openBinaryFileForWriting :: FilePath -> IO Handle
openBinaryFileForWriting path = do
  handle <- openBinaryFile path WriteMode
  return handle

-- Чтение блока бинарных данных из Handle
readBinaryBlock :: Handle -> Int -> IO BS.ByteString
readBinaryBlock handle size = BS.hGet handle size

-- Запись блока бинарных данных в Handle
writeBinaryBlock :: Handle -> BS.ByteString -> IO ()
writeBinaryBlock handle content = BS.hPut handle content

-- Пример 3: Сериализация и десериализация с помощью Data.Binary

-- Определение типа данных для сериализации
data Person = Person
  { name :: String
  , age :: Int
  , email :: String
  } deriving (Show, Eq)

-- Реализация экземпляра Binary для Person
instance Binary Person where
  put person = do
    put (name person)
    put (age person)
    put (email person)
  
  get = do
    name' <- get
    age' <- get
    email' <- get
    return $ Person name' age' email'

-- Сериализация данных в ByteString
serializeData :: Binary a => a -> BL.ByteString
serializeData = encode

-- Десериализация данных из ByteString
deserializeData :: Binary a => BL.ByteString -> a
deserializeData = decode

-- Сохранение сериализованных данных в файл
saveDataToFile :: Binary a => FilePath -> a -> IO ()
saveDataToFile path data' = do
  let encoded = serializeData data'
  writeLazyBinaryFile path encoded

-- Загрузка сериализованных данных из файла
loadDataFromFile :: Binary a => FilePath -> IO a
loadDataFromFile path = do
  encoded <- readLazyBinaryFile path
  return $ deserializeData encoded

-- Пример 4: Низкоуровневая работа с бинарными данными

-- Упаковка целого числа в 4 байта (big-endian)
packInt32BE :: Int32 -> BS.ByteString
packInt32BE n = BS.pack [
    fromIntegral (n `shiftR` 24) .&. 0xFF,
    fromIntegral (n `shiftR` 16) .&. 0xFF,
    fromIntegral (n `shiftR` 8) .&. 0xFF,
    fromIntegral n .&. 0xFF
  ]

-- Распаковка 4 байт в целое число (big-endian)
unpackInt32BE :: BS.ByteString -> Int32
unpackInt32BE bs
  | BS.length bs /= 4 = error "ByteString должен содержать ровно 4 байта"
  | otherwise = 
      (fromIntegral (BS.index bs 0) `shiftL` 24) .|.
      (fromIntegral (BS.index bs 1) `shiftL` 16) .|.
      (fromIntegral (BS.index bs 2) `shiftL` 8) .|.
      fromIntegral (BS.index bs 3)

-- Упаковка целого числа в 4 байта (little-endian)
packInt32LE :: Int32 -> BS.ByteString
packInt32LE n = BS.pack [
    fromIntegral n .&. 0xFF,
    fromIntegral (n `shiftR` 8) .&. 0xFF,
    fromIntegral (n `shiftR` 16) .&. 0xFF,
    fromIntegral (n `shiftR` 24) .&. 0xFF
  ]

-- Распаковка 4 байт в целое число (little-endian)
unpackInt32LE :: BS.ByteString -> Int32
unpackInt32LE bs
  | BS.length bs /= 4 = error "ByteString должен содержать ровно 4 байта"
  | otherwise = 
      fromIntegral (BS.index bs 0) .|.
      (fromIntegral (BS.index bs 1) `shiftL` 8) .|.
      (fromIntegral (BS.index bs 2) `shiftL` 16) .|.
      (fromIntegral (BS.index bs 3) `shiftL` 24)

-- Пример 5: Использование Data.Binary.Get и Data.Binary.Put

-- Создание бинарного формата с помощью Put
createBinaryFormat :: Put
createBinaryFormat = do
  putWord8 0x42  -- Магическое число (B)
  putWord8 0x49  -- Магическое число (I)
  putWord8 0x4E  -- Магическое число (N)
  putWord32be 0x00000001  -- Версия формата
  putWord16be 0x0003  -- Количество записей
  
  -- Запись 1
  putWord8 0x01  -- Тип записи
  putWord16be 0x000A  -- Длина данных
  putByteString $ BS.pack $ map (fromIntegral . ord) "Hello, World"
  
  -- Запись 2
  putWord8 0x02  -- Тип записи
  putWord16be 0x0004  -- Длина данных
  putWord32be 0x12345678  -- Данные
  
  -- Запись 3
  putWord8 0x03  -- Тип записи
  putWord16be 0x0008  -- Длина данных
  putWord32be 0xAABBCCDD  -- Данные 1
  putWord32be 0x11223344  -- Данные 2

-- Парсинг бинарного формата с помощью Get
parseBinaryFormat :: Get (BS.ByteString, Word32, Word32)
parseBinaryFormat = do
  -- Проверка магического числа
  magic1 <- getWord8
  magic2 <- getWord8
  magic3 <- getWord8
  when (magic1 /= 0x42 || magic2 /= 0x49 || magic3 /= 0x4E) $
    fail "Неверное магическое число"
  
  -- Проверка версии
  version <- getWord32be
  when (version /= 0x00000001) $
    fail "Неподдерживаемая версия формата"
  
  -- Чтение количества записей
  recordCount <- getWord16be
  when (recordCount /= 0x0003) $
    fail "Неверное количество записей"
  
  -- Чтение записи 1
  recordType1 <- getWord8
  when (recordType1 /= 0x01) $
    fail "Неверный тип записи 1"
  
  dataLength1 <- getWord16be
  stringData <- getByteString (fromIntegral dataLength1)
  
  -- Чтение записи 2
  recordType2 <- getWord8
  when (recordType2 /= 0x02) $
    fail "Неверный тип записи 2"
  
  dataLength2 <- getWord16be
  when (dataLength2 /= 0x0004) $
    fail "Неверная длина данных записи 2"
  
  intData <- getWord32be
  
  -- Чтение записи 3
  recordType3 <- getWord8
  when (recordType3 /= 0x03) $
    fail "Неверный тип записи 3"
  
  dataLength3 <- getWord16be
  when (dataLength3 /= 0x0008) $
    fail "Неверная длина данных записи 3"
  
  intData1 <- getWord32be
  intData2 <- getWord32be
  
  return (stringData, intData1, intData2)

-- Пример 6: Работа с бинарными протоколами

-- Определение простого бинарного протокола
data Message = 
    Ping Word32
  | Pong Word32
  | Data BS.ByteString
  | Error String
  deriving (Show, Eq)

-- Сериализация сообщения
serializeMessage :: Message -> BL.ByteString
serializeMessage msg = runPut $ case msg of
  Ping n -> do
    putWord8 0x01  -- Тип сообщения: Ping
    putWord32be n
  
  Pong n -> do
    putWord8 0x02  -- Тип сообщения: Pong
    putWord32be n
  
  Data bs -> do
    putWord8 0x03  -- Тип сообщения: Data
    putWord32be (fromIntegral $ BS.length bs)
    putByteString bs
  
  Error str -> do
    putWord8 0x04  -- Тип сообщения: Error
    let bs = BS.pack $ map (fromIntegral . ord) str
    putWord32be (fromIntegral $ BS.length bs)
    putByteString bs

-- Десериализация сообщения
deserializeMessage :: BL.ByteString -> Either String Message
deserializeMessage bs = case runGetOrFail parseMessage bs of
  Left (_, _, err) -> Left err
  Right (_, _, msg) -> Right msg

-- Парсинг сообщения
parseMessage :: Get Message
parseMessage = do
  msgType <- getWord8
  case msgType of
    0x01 -> do
      n <- getWord32be
      return $ Ping n
    
    0x02 -> do
      n <- getWord32be
      return $ Pong n
    
    0x03 -> do
      len <- getWord32be
      bs <- getByteString (fromIntegral len)
      return $ Data bs
    
    0x04 -> do
      len <- getWord32be
      bs <- getByteString (fromIntegral len)
      let str = map (chr . fromIntegral) $ BS.unpack bs
      return $ Error str
    
    _ -> fail $ "Неизвестный тип сообщения: " ++ show msgType

-- Пример 7: Работа с бинарными файлами изображений

-- Определение формата простого изображения
data Image = Image
  { width :: Word32
  , height :: Word32
  , pixels :: [[Word8]]  -- Значения яркости пикселей (0-255)
  } deriving (Show, Eq)

-- Создание пустого изображения заданного размера
createEmptyImage :: Word32 -> Word32 -> Image
createEmptyImage w h = Image
  { width = w
  , height = h
  , pixels = replicate (fromIntegral h) (replicate (fromIntegral w) 0)
  }

-- Сериализация изображения в бинарный формат
serializeImage :: Image -> BL.ByteString
serializeImage img = runPut $ do
  putWord32be (width img)
  putWord32be (height img)
  forM_ (pixels img) $ \row ->
    forM_ row putWord8

-- Десериализация изображения из бинарного формата
deserializeImage :: BL.ByteString -> Either String Image
deserializeImage bs = case runGetOrFail parseImage bs of
  Left (_, _, err) -> Left err
  Right (_, _, img) -> Right img

-- Парсинг изображения
parseImage :: Get Image
parseImage = do
  w <- getWord32be
  h <- getWord32be
  rows <- replicateM (fromIntegral h) $
    replicateM (fromIntegral w) getWord8
  return $ Image w h rows

-- Пример 1: Базовые операции с бинарными файлами
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Базовые операции с бинарными файлами"
  
  -- Создание временного бинарного файла
  let tempFile = "temp_binary.bin"
  
  -- Запись бинарных данных в файл
  putStrLn "Запись бинарных данных в файл..."
  let binaryData = BS.pack [0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x2C, 0x20, 0x57, 0x6F, 0x72, 0x6C, 0x64]  -- "Hello, World"
  writeBinaryFile tempFile binaryData
  
  -- Чтение бинарных данных из файла
  putStrLn "Чтение бинарных данных из файла:"
  content <- readBinaryFile tempFile
  putStrLn $ "Прочитано " ++ show (BS.length content) ++ " байт: " ++ show content
  
  -- Преобразование бинарных данных в строку
  let string = map (chr . fromIntegral) $ BS.unpack content
  putStrLn $ "Как строка: " ++ string
  
  -- Запись Lazy ByteString в файл
  putStrLn "\nЗапись Lazy ByteString в файл..."
  let lazyData = BL.pack [0x57, 0x6F, 0x72, 0x6C, 0x64, 0x2C, 0x20, 0x48, 0x65, 0x6C, 0x6C, 0x6F]  -- "World, Hello"
  writeLazyBinaryFile tempFile lazyData
  
  -- Чтение Lazy ByteString из файла
  putStrLn "Чтение Lazy ByteString из файла:"
  lazyContent <- readLazyBinaryFile tempFile
  putStrLn $ "Прочитано " ++ show (BL.length lazyContent) ++ " байт: " ++ show lazyContent
  
  -- Преобразование Lazy ByteString в строку
  let lazyString = map (chr . fromIntegral) $ BL.unpack lazyContent
  putStrLn $ "Как строка: " ++ lazyString
  
  putStrLn "Временный файл создан: temp_binary.bin"

-- Пример 2: Работа с бинарными данными через Handle
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Работа с бинарными данными через Handle"
  
  -- Создание временного бинарного файла
  let tempFile = "temp_binary_handle.bin"
  
  -- Запись бинарных данных через Handle
  putStrLn "Запись бинарных данных через Handle..."
  bracket
    (openBinaryFileForWriting tempFile)
    hClose
    (\handle -> do
      -- Запись нескольких блоков данных
      writeBinaryBlock handle $ BS.pack [0x01, 0x02, 0x03, 0x04]
      writeBinaryBlock handle $ BS.pack [0x05, 0x06, 0x07, 0x08]
      writeBinaryBlock handle $ BS.pack [0x09, 0x0A, 0x0B, 0x0C]
    )
  
  -- Чтение бинарных данных через Handle
  putStrLn "Чтение бинарных данных через Handle:"
  bracket
    (openBinaryFileForReading tempFile)
    hClose
    (\handle -> do
      -- Чтение блоков данных
      block1 <- readBinaryBlock handle 4
      putStrLn $ "Блок 1: " ++ show block1
      
      block2 <- readBinaryBlock handle 4
      putStrLn $ "Блок 2: " ++ show block2
      
      block3 <- readBinaryBlock handle 4
      putStrLn $ "Блок 3: " ++ show block3
    )
  
  putStrLn "Временный файл создан: temp_binary_handle.bin"

-- Пример 3: Сериализация и десериализация с помощью Data.Binary
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Сериализация и десериализация с помощью Data.Binary"
  
  -- Создание временного файла для сериализованных данных
  let tempFile = "temp_serialized.bin"
  
  -- Создание объекта Person
  let person = Person
        { name = "Иван Иванов"
        , age = 30
        , email = "ivan@example.com"
        }
  
  putStrLn $ "Исходный объект: " ++ show person
  
  -- Сериализация объекта в ByteString
  putStrLn "\nСериализация объекта в ByteString..."
  let serialized = serializeData person
  putStrLn $ "Размер сериализованных данных: " ++ show (BL.length serialized) ++ " байт"
  
  -- Сохранение сериализованных данных в файл
  putStrLn "Сохранение сериализованных данных в файл..."
  saveDataToFile tempFile person
  
  -- Загрузка сериализованных данных из файла
  putStrLn "Загрузка сериализованных данных из файла..."
  loadedPerson <- loadDataFromFile tempFile :: IO Person
  
  -- Проверка результата
  putStrLn $ "Загруженный объект: " ++ show loadedPerson
  putStrLn $ "Объекты идентичны: " ++ show (person == loadedPerson)
  
  putStrLn "Временный файл создан: temp_serialized.bin"

-- Пример 4: Низкоуровневая работа с бинарными данными
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Низкоуровневая работа с бинарными данными"
  
  -- Упаковка и распаковка целых чисел
  let number = 0x12345678 :: Int32
  
  putStrLn $ "Исходное число: " ++ show number ++ " (0x" ++ showHex number ++ ")"
  
  -- Упаковка в big-endian
  let packedBE = packInt32BE number
  putStrLn $ "Упаковано в big-endian: " ++ show packedBE
  
  -- Распаковка из big-endian
  let unpackedBE = unpackInt32BE packedBE
  putStrLn $ "Распаковано из big-endian: " ++ show unpackedBE ++ " (0x" ++ showHex unpackedBE ++ ")"
  
  -- Упаковка в little-endian
  let packedLE = packInt32LE number
  putStrLn $ "Упаковано в little-endian: " ++ show packedLE
  
  -- Распаковка из little-endian
  let unpackedLE = unpackInt32LE packedLE
  putStrLn $ "Распаковано из little-endian: " ++ show unpackedLE ++ " (0x" ++ showHex unpackedLE ++ ")"
  
  -- Создание временного файла для бинарных данных
  let tempFile = "temp_binary_raw.bin"
  
  -- Запись упакованных данных в файл
  putStrLn "\nЗапись упакованных данных в файл..."
  BS.writeFile tempFile $ BS.append packedBE packedLE
  
  -- Чтение упакованных данных из файла
  putStrLn "Чтение упакованных данных из файла..."
  fileContent <- BS.readFile tempFile
  
  -- Разделение данных на две части
  let (firstHalf, secondHalf) = BS.splitAt 4 fileContent
  
  -- Распаковка данных
  let firstNumber = unpackInt32BE firstHalf
  let secondNumber = unpackInt32LE secondHalf
  
  putStrLn $ "Первое число (big-endian): " ++ show firstNumber ++ " (0x" ++ showHex firstNumber ++ ")"
  putStrLn $ "Второе число (little-endian): " ++ show secondNumber ++ " (0x" ++ showHex secondNumber ++ ")"
  
  putStrLn "Временный файл создан: temp_binary_raw.bin"
  where
    showHex n = showHex' (fromIntegral n :: Word32) ""
    showHex' 0 s = '0' : s
    showHex' n s = showHex' (n `div` 16) (hexDigit (n `mod` 16) : s)
    hexDigit d
      | d < 10    = chr (ord '0' + fromIntegral d)
      | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- Пример 5: Использование Data.Binary.Get и Data.Binary.Put
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Использование Data.Binary.Get и Data.Binary.Put"
  
  -- Создание бинарных данных с помощью Put
  putStrLn "Создание бинарных данных с помощью Put..."
  let binaryData = runPut createBinaryFormat
  
  -- Вывод размера данных
  putStrLn $ "Размер бинарных данных: " ++ show (BL.length binaryData) ++ " байт"
  
  -- Сохранение бинарных данных в файл
  let tempFile = "temp_binary_format.bin"
  putStrLn $ "Сохранение бинарных данных в файл: " ++ tempFile
  BL.writeFile tempFile binaryData
  
  -- Чтение бинарных данных из файла
  putStrLn "Чтение бинарных данных из файла..."
  fileContent <- BL.readFile tempFile
  
  -- Парсинг бинарных данных с помощью Get
  putStrLn "Парсинг бинарных данных с помощью Get..."
  case runGetOrFail parseBinaryFormat fileContent of
    Left (_, _, err) -> putStrLn $ "Ошибка парсинга: " ++ err
    Right (_, _, (stringData, intData1, intData2)) -> do
      putStrLn "Парсинг успешен!"
      putStrLn $ "Строковые данные: " ++ map (chr . fromIntegral) (BS.unpack stringData)
      putStrLn $ "Целочисленные данные 1: 0x" ++ showHex intData1
      putStrLn $ "Целочисленные данные 2: 0x" ++ showHex intData2
  
  putStrLn "Временный файл создан: temp_binary_format.bin"
  where
    showHex n = showHex' n ""
    showHex' 0 s = '0' : s
    showHex' n s = showHex' (n `div` 16) (hexDigit (n `mod` 16) : s)
    hexDigit d
      | d < 10    = chr (ord '0' + fromIntegral d)
      | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- Пример 6: Работа с бинарными протоколами
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Работа с бинарными протоколами"
  
  -- Создание сообщений
  let pingMsg = Ping 12345
  let pongMsg = Pong 54321
  let dataMsg = Data $ BS.pack [0x01, 0x02, 0x03, 0x04, 0x05]
  let errorMsg = Error "Произошла ошибка"
  
  -- Сериализация сообщений
  putStrLn "Сериализация сообщений..."
  let serializedPing = serializeMessage pingMsg
  let serializedPong = serializeMessage pongMsg
  let serializedData = serializeMessage dataMsg
  let serializedError = serializeMessage errorMsg
  
  putStrLn $ "Размер сериализованного Ping: " ++ show (BL.length serializedPing) ++ " байт"
  putStrLn $ "Размер сериализованного Pong: " ++ show (BL.length serializedPong) ++ " байт"
  putStrLn $ "Размер сериализованного Data: " ++ show (BL.length serializedData) ++ " байт"
  putStrLn $ "Размер сериализованного Error: " ++ show (BL.length serializedError) ++ " байт"
  
  -- Сохранение сериализованных сообщений в файл
  let tempFile = "temp_protocol.bin"
  putStrLn $ "\nСохранение сериализованных сообщений в файл: " ++ tempFile
  BL.writeFile tempFile $ BL.append (BL.append serializedPing serializedPong) 
                                    (BL.append serializedData serializedError)
  
  -- Чтение сериализованных сообщений из файла
  putStrLn "Чтение сериализованных сообщений из файла..."
  fileContent <- BL.readFile tempFile
  
  -- Десериализация сообщений
  putStrLn "Десериализация сообщений..."
  
  -- Разделение файла на части для каждого сообщения
  let (pingContent, rest1) = BL.splitAt 5 fileContent  -- Ping: 1 байт тип + 4 байта данные
  let (pongContent, rest2) = BL.splitAt 5 rest1        -- Pong: 1 байт тип + 4 байта данные
  let (dataContent, errorContent) = BL.splitAt 10 rest2  -- Data: 1 байт тип + 4 байта длина + 5 байт данные
  
  -- Десериализация каждого сообщения
  case deserializeMessage pingContent of
    Left err -> putStrLn $ "Ошибка десериализации Ping: " ++ err
    Right msg -> putStrLn $ "Десериализованный Ping: " ++ show msg
  
  case deserializeMessage pongContent of
    Left err -> putStrLn $ "Ошибка десериализации Pong: " ++ err
    Right msg -> putStrLn $ "Десериализованный Pong: " ++ show msg
  
  case deserializeMessage dataContent of
    Left err -> putStrLn $ "Ошибка десериализации Data: " ++ err
    Right msg -> putStrLn $ "Десериализованный Data: " ++ show msg
  
  case deserializeMessage errorContent of
    Left err -> putStrLn $ "Ошибка десериализации Error: " ++ err
    Right msg -> putStrLn $ "Десериализованный Error: " ++ show msg
  
  putStrLn "Временный файл создан: temp_protocol.bin"

-- Пример 7: Работа с бинарными файлами изображений
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Работа с бинарными файлами изображений"
  
  -- Создание простого изображения
  putStrLn "Создание простого изображения..."
  let img = createEmptyImage 4 3
  
  -- Модификация пикселей изображения
  let modifiedImg = img { pixels = 
        [ [255, 0, 0, 255]
        , [0, 255, 0, 255]
        , [0, 0, 255, 255]
        ] }
  
  putStrLn $ "Размер изображения: " ++ show (width modifiedImg) ++ "x" ++ show (height modifiedImg)
  
  -- Вывод пикселей изображения
  putStrLn "Пиксели изображения:"
  forM_ (pixels modifiedImg) $ \row -> do
    putStrLn $ concatMap (\p -> if p > 0 then "X " else ". ") row
  
  -- Сериализация изображения
  putStrLn "\nСериализация изображения..."
  let serializedImg = serializeImage modifiedImg
  putStrLn $ "Размер сериализованного изображения: " ++ show (BL.length serializedImg) ++ " байт"
  
  -- Сохранение сериализованного изображения в файл
  let tempFile = "temp_image.bin"
  putStrLn $ "Сохранение сериализованного изображения в файл: " ++ tempFile
  BL.writeFile tempFile serializedImg
  
  -- Чтение сериализованного изображения из файла
  putStrLn "Чтение сериализованного изображения из файла..."
  fileContent <- BL.readFile tempFile
  
  -- Десериализация изображения
  putStrLn "Десериализация изображения..."
  case deserializeImage fileContent of
    Left err -> putStrLn $ "Ошибка десериализации: " ++ err
    Right img' -> do
      putStrLn "Десериализация успешна!"
      putStrLn $ "Размер изображения: " ++ show (width img') ++ "x" ++ show (height img')
      
      -- Вывод пикселей десериализованного изображения
      putStrLn "Пиксели десериализованного изображения:"
      forM_ (pixels img') $ \row -> do
        putStrLn $ concatMap (\p -> if p > 0 then "X " else ". ") row
      
      -- Проверка, что изображения идентичны
      putStrLn $ "Изображения идентичны: " ++ show (modifiedImg == img')
  
  putStrLn "Временный файл создан: temp_image.bin"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Работа с бинарными данными в Haskell\n"
  
  -- Запуск всех примеров
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  
  putStrLn "\nКлючевые моменты о работе с бинарными данными в Haskell:"
  putStrLn "1. Модули Data.ByteString и Data.ByteString.Lazy предоставляют эффективные структуры данных для работы с бинарными данными"
  putStrLn "2. Модуль Data.Binary предоставляет функции для сериализации и десериализации данных"
  putStrLn "3. Модули Data.Binary.Get и Data.Binary.Put предоставляют низкоуровневый доступ к бинарным данным"
  putStrLn "4. Бинарные данные могут быть представлены в разных форматах (big-endian, little-endian)"
  putStrLn "5. Бинарные протоколы могут быть реализованы с помощью функций сериализации и десериализации"
  putStrLn "6. Бинарные файлы могут быть прочитаны и записаны с помощью функций из модуля System.IO"
  putStrLn "7. Бинарные данные могут быть обработаны с помощью функций из модуля Data.Bits"
  
  -- Удаление временных файлов
  putStrLn "\nУдаление временных файлов..."
  removeIfExists "temp_binary.bin"
  removeIfExists "temp_binary_handle.bin"
  removeIfExists "temp_serialized.bin"
  removeIfExists "temp_binary_raw.bin"
  removeIfExists "temp_binary_format.bin"
  removeIfExists "temp_protocol.bin"
  removeIfExists "temp_image.bin"
  putStrLn "Временные файлы удалены."
  where
    removeIfExists path = do
      exists <- doesFileExist path
      when exists $ do
        removeFile path
        putStrLn $ "Удален файл: " ++ path
