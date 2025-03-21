{-
  Основы ввода-вывода в Haskell
  
  В этом файле мы рассмотрим основы ввода-вывода (IO) в Haskell,
  включая монаду IO, do-нотацию и базовые операции ввода-вывода.
-}

module Main where

import Control.Monad (when, forM_)
import Data.Char (toUpper)

-- Ввод-вывод в Haskell реализован через монаду IO.
-- Монада IO позволяет выполнять операции с побочными эффектами
-- в чистом функциональном языке.

-- Пример 1: Простой вывод на экран

-- Функция putStrLn выводит строку на экран и добавляет перевод строки
helloWorld :: IO ()
helloWorld = putStrLn "Привет, мир!"

-- Функция putStr выводит строку на экран без перевода строки
helloWithoutNewline :: IO ()
helloWithoutNewline = putStr "Привет, "

-- Функция print выводит значение, преобразуя его в строку с помощью show
printValue :: Show a => a -> IO ()
printValue = print

-- Пример 2: Ввод с клавиатуры

-- Функция getLine считывает строку с клавиатуры
readName :: IO String
readName = do
  putStr "Введите ваше имя: "
  getLine

-- Функция getChar считывает один символ с клавиатуры
readChar :: IO Char
readChar = do
  putStr "Введите символ: "
  getChar

-- Функция read преобразует строку в значение указанного типа
readNumber :: IO Int
readNumber = do
  putStr "Введите число: "
  input <- getLine
  -- Безопасное преобразование строки в число с обработкой ошибок
  case reads input of
    [(n, "")] -> return n
    _         -> do
      putStrLn "Ошибка: введите корректное целое число"
      readNumber

-- Пример 3: do-нотация

-- do-нотация - это синтаксический сахар для работы с монадами
-- Она позволяет писать код в императивном стиле
greet :: IO ()
greet = do
  putStrLn "Как вас зовут?"
  name <- getLine
  putStrLn $ "Привет, " ++ name ++ "!"

-- Эквивалентная запись без do-нотации
greetWithoutDo :: IO ()
greetWithoutDo =
  putStrLn "Как вас зовут?" >>
  getLine >>= \name ->
  putStrLn $ "Привет, " ++ name ++ "!"

-- Пример 4: Комбинирование IO-действий

-- Последовательное выполнение IO-действий
sequence1 :: IO ()
sequence1 = do
  putStrLn "Первое действие"
  putStrLn "Второе действие"
  putStrLn "Третье действие"

-- Условное выполнение IO-действий
conditional :: IO ()
conditional = do
  putStr "Введите число: "
  input <- getLine
  -- Безопасное преобразование строки в число с обработкой ошибок
  case reads input of
    [(number, "")] -> do
      when (number > 0) $ do
        putStrLn "Число положительное"
        putStrLn $ "Квадрат числа: " ++ show (number * number)
      when (number <= 0) $ do
        putStrLn "Число не положительное"
    _ -> do
      putStrLn "Ошибка: введите корректное целое число"
      conditional

-- Циклическое выполнение IO-действий
loop :: IO ()
loop = do
  putStrLn "Введите числа (пустая строка для завершения):"
  loopNumbers 0
  where
    loopNumbers sum = do
      input <- getLine
      if input == ""
        then putStrLn $ "Сумма: " ++ show sum
        else case reads input of
          [(number, "")] -> loopNumbers (sum + number)
          _ -> do
            putStrLn "Ошибка: введите корректное целое число"
            loopNumbers sum

-- Использование forM_ для итерации по списку
iterateList :: IO ()
iterateList = do
  let numbers = [1..5]
  forM_ numbers $ \n -> do
    putStrLn $ "Число: " ++ show n
    putStrLn $ "Квадрат: " ++ show (n * n)

-- Пример 5: Возвращение значений из IO-действий

-- Функция return создает IO-действие, которое возвращает значение
returnValue :: IO Int
returnValue = do
  putStrLn "Возвращаем число 42"
  return 42

-- Использование возвращаемого значения
useReturnValue :: IO ()
useReturnValue = do
  value <- returnValue
  putStrLn $ "Полученное значение: " ++ show value

-- Преобразование ввода
transformInput :: IO String
transformInput = do
  putStr "Введите строку: "
  input <- getLine
  return (map toUpper input)

-- Пример 6: Чистые функции и IO

-- Чистая функция (без побочных эффектов)
square :: Int -> Int
square x = x * x

-- Использование чистой функции в IO-контексте
useSquare :: IO ()
useSquare = do
  putStr "Введите число: "
  input <- getLine
  -- Безопасное преобразование строки в число с обработкой ошибок
  case reads input of
    [(number, "")] -> do
      let result = square number
      putStrLn $ "Квадрат числа: " ++ show result
    _ -> do
      putStrLn "Ошибка: введите корректное целое число"
      useSquare

-- Комбинирование чистых функций и IO
processInput :: String -> String
processInput = map toUpper

useProcessInput :: IO ()
useProcessInput = do
  putStr "Введите строку: "
  input <- getLine
  let processed = processInput input
  putStrLn $ "Результат: " ++ processed

-- Пример 7: Побочные эффекты и чистота

-- В Haskell функции с побочными эффектами имеют тип IO a
-- Это позволяет отделить чистый код от кода с побочными эффектами

-- Функция с побочным эффектом (вывод на экран)
printMessage :: String -> IO ()
printMessage message = putStrLn message

-- Чистая функция, которая не выполняет ввод-вывод
pureFunction :: Int -> Int
pureFunction x = x * 2

-- Функция, которая комбинирует чистый код и код с побочными эффектами
combined :: Int -> IO Int
combined x = do
  let result = pureFunction x
  printMessage $ "Результат: " ++ show result
  return result

-- Пример 1: Простой вывод на экран
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Простой вывод на экран"
  
  putStrLn "Вызов helloWorld:"
  helloWorld
  
  putStrLn "Вызов helloWithoutNewline и затем putStrLn:"
  helloWithoutNewline
  putStrLn "мир!"
  
  putStrLn "Вызов printValue с разными типами:"
  printValue (42 :: Int)
  printValue (3.14 :: Double)
  printValue "строка"
  printValue True

-- Пример 2: Ввод с клавиатуры
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Ввод с клавиатуры"
  
  putStrLn "Вызов readName:"
  name <- readName
  putStrLn $ "Вы ввели: " ++ name
  
  putStrLn "Вызов readChar:"
  char <- readChar
  putStrLn $ "Вы ввели символ: " ++ [char]
  
  putStrLn "Вызов readNumber:"
  number <- readNumber
  putStrLn $ "Вы ввели число: " ++ show number

-- Пример 3: do-нотация
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: do-нотация"
  
  putStrLn "Вызов greet (с do-нотацией):"
  greet
  
  putStrLn "Вызов greetWithoutDo (без do-нотации):"
  greetWithoutDo

-- Пример 4: Комбинирование IO-действий
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Комбинирование IO-действий"
  
  putStrLn "Вызов sequence1:"
  sequence1
  
  putStrLn "Вызов conditional:"
  conditional
  
  putStrLn "Вызов iterateList:"
  iterateList
  
  putStrLn "Вызов loop:"
  loop

-- Пример 5: Возвращение значений из IO-действий
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Возвращение значений из IO-действий"
  
  putStrLn "Вызов useReturnValue:"
  useReturnValue
  
  putStrLn "Вызов transformInput:"
  result <- transformInput
  putStrLn $ "Преобразованная строка: " ++ result

-- Пример 6: Чистые функции и IO
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Чистые функции и IO"
  
  putStrLn "Вызов useSquare:"
  useSquare
  
  putStrLn "Вызов useProcessInput:"
  useProcessInput

-- Пример 7: Побочные эффекты и чистота
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Побочные эффекты и чистота"
  
  putStrLn "Вызов printMessage:"
  printMessage "Это сообщение выводится с помощью функции с побочным эффектом"
  
  putStrLn "Вызов combined:"
  result <- combined 21
  putStrLn $ "Полученное значение: " ++ show result

-- Функция для демонстрации всех примеров без интерактивного ввода
demoAllExamples :: IO ()
demoAllExamples = do
  -- Пример 1: Простой вывод на экран
  putStrLn "Пример 1: Простой вывод на экран"
  helloWorld
  helloWithoutNewline
  putStrLn "мир!"
  printValue (42 :: Int)
  printValue (3.14 :: Double)
  printValue "строка"
  printValue True
  
  -- Пример 2: Ввод с клавиатуры (демо без реального ввода)
  putStrLn "\nПример 2: Ввод с клавиатуры (демо)"
  putStrLn "Функция readName запрашивает имя пользователя"
  putStrLn "Функция readChar запрашивает один символ"
  putStrLn "Функция readNumber запрашивает число и преобразует его в Int"
  
  -- Пример 3: do-нотация (демо без реального ввода)
  putStrLn "\nПример 3: do-нотация (демо)"
  putStrLn "Функция greet запрашивает имя и приветствует пользователя"
  putStrLn "Функция greetWithoutDo делает то же самое, но без do-нотации"
  
  -- Пример 4: Комбинирование IO-действий
  putStrLn "\nПример 4: Комбинирование IO-действий"
  sequence1
  putStrLn "Функция conditional запрашивает число и выполняет действия в зависимости от его значения"
  iterateList
  putStrLn "Функция loop запрашивает числа и вычисляет их сумму"
  
  -- Пример 5: Возвращение значений из IO-действий
  putStrLn "\nПример 5: Возвращение значений из IO-действий"
  useReturnValue
  putStrLn "Функция transformInput запрашивает строку и преобразует ее в верхний регистр"
  
  -- Пример 6: Чистые функции и IO
  putStrLn "\nПример 6: Чистые функции и IO"
  putStrLn "Функция useSquare запрашивает число и вычисляет его квадрат"
  putStrLn "Функция useProcessInput запрашивает строку и преобразует ее в верхний регистр"
  
  -- Пример 7: Побочные эффекты и чистота
  putStrLn "\nПример 7: Побочные эффекты и чистота"
  printMessage "Это сообщение выводится с помощью функции с побочным эффектом"
  result <- combined 21
  putStrLn $ "Полученное значение: " ++ show result

-- Функция для запуска конкретного примера
runExample :: Int -> IO ()
runExample 1 = example1
runExample 2 = example2
runExample 3 = example3
runExample 4 = example4
runExample 5 = example5
runExample 6 = example6
runExample 7 = example7
runExample _ = putStrLn "Неверный номер примера. Доступны примеры с 1 по 7."

-- Функция для отображения меню
showMenu :: IO ()
showMenu = do
  putStrLn "\nВыберите действие:"
  putStrLn "1. Запустить пример 1: Простой вывод на экран"
  putStrLn "2. Запустить пример 2: Ввод с клавиатуры"
  putStrLn "3. Запустить пример 3: do-нотация"
  putStrLn "4. Запустить пример 4: Комбинирование IO-действий"
  putStrLn "5. Запустить пример 5: Возвращение значений из IO-действий"
  putStrLn "6. Запустить пример 6: Чистые функции и IO"
  putStrLn "7. Запустить пример 7: Побочные эффекты и чистота"
  putStrLn "8. Демонстрация всех примеров (без интерактивного ввода)"
  putStrLn "9. Показать ключевые моменты"
  putStrLn "0. Выход"
  putStr "Ваш выбор: "

-- Функция для отображения ключевых моментов
showKeyPoints :: IO ()
showKeyPoints = do
  putStrLn "\nКлючевые моменты о вводе-выводе в Haskell:"
  putStrLn "1. Ввод-вывод в Haskell реализован через монаду IO"
  putStrLn "2. Монада IO позволяет выполнять операции с побочными эффектами в чистом функциональном языке"
  putStrLn "3. do-нотация - это синтаксический сахар для работы с монадами, делает код более читаемым"
  putStrLn "4. Функции с побочными эффектами имеют тип IO a"
  putStrLn "5. Чистые функции можно комбинировать с IO-действиями"
  putStrLn "6. return создает IO-действие, которое возвращает значение"
  putStrLn "7. Разделение чистого кода и кода с побочными эффектами - важный принцип в Haskell"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Основы ввода-вывода в Haskell\n"
  
  -- Интерактивное меню
  menuLoop
  
  putStrLn "Программа завершена."
  where
    menuLoop = do
      showMenu
      choice <- getLine
      case choice of
        "0" -> return ()
        "1" -> runExample 1 >> menuLoop
        "2" -> runExample 2 >> menuLoop
        "3" -> runExample 3 >> menuLoop
        "4" -> runExample 4 >> menuLoop
        "5" -> runExample 5 >> menuLoop
        "6" -> runExample 6 >> menuLoop
        "7" -> runExample 7 >> menuLoop
        "8" -> demoAllExamples >> menuLoop
        "9" -> showKeyPoints >> menuLoop
        _   -> putStrLn "Неверный выбор. Попробуйте снова." >> menuLoop
