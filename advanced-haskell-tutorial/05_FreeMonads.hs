
{-
  Free монады в Haskell
  
  В этом файле мы рассмотрим Free монады - способ построения монад из функторов
  и разделения описания и интерпретации эффектов.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (ap, liftM)
import Data.Functor.Identity
import System.IO hiding (readFile')

-- Часть 1: Введение в Free монады
-- -----------------------------

-- Free монада - это способ превратить любой функтор в монаду.
-- Она позволяет разделить описание вычисления и его интерпретацию.

-- Определение Free монады
data Free f a
  = Pure a
  | Free (f (Free f a))

-- Реализация Functor для Free
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

-- Реализация Applicative для Free
instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

-- Реализация Monad для Free
instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free fa >>= f = Free (fmap (>>= f) fa)

-- Функция для подъема функтора в Free монаду
liftF :: Functor f => f a -> Free f a
liftF fa = Free (fmap Pure fa)

-- Функция для интерпретации Free монады
foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = return a
foldFree f (Free fa) = f fa >>= foldFree f

-- Часть 2: Пример использования Free монады для консольного ввода-вывода
-- ------------------------------------------------------------------

-- Определение функтора для консольного ввода-вывода
data ConsoleF a
  = PutStrLn String a
  | GetLine (String -> a)
  | Exit
  deriving Functor

-- Тип для программы, использующей консольный ввод-вывод
type Console a = Free ConsoleF a

-- Умные конструкторы для консольных операций
putStrLn' :: String -> Console ()
putStrLn' s = liftF (PutStrLn s ())

getLine' :: Console String
getLine' = liftF (GetLine id)

exit :: Console a
exit = liftF Exit

-- Пример программы, использующей консольный ввод-вывод
greet :: Console ()
greet = do
  putStrLn' "Как вас зовут?"
  name <- getLine'
  putStrLn' $ "Привет, " ++ name ++ "!"
  putStrLn' "Сколько вам лет?"
  ageStr <- getLine'
  let age = read ageStr :: Int
  if age < 18
    then putStrLn' "Вы еще молоды!"
    else putStrLn' "Вы уже взрослый!"
  putStrLn' "До свидания!"

-- Интерпретатор для консольного ввода-вывода в IO
runConsoleIO :: Console a -> IO a
runConsoleIO = foldFree $ \case
  PutStrLn s next -> do
    putStrLn s
    return next
  GetLine f -> do
    input <- getLine
    return (f input)
  Exit -> error "Exit"

-- Интерпретатор для тестирования консольного ввода-вывода
runConsolePure :: [String] -> Console a -> ([String], Maybe a)
runConsolePure inputs = go inputs []
  where
    go :: [String] -> [String] -> Console a -> ([String], Maybe a)
    go _ outputs (Pure a) = (reverse outputs, Just a)
    go inputs outputs (Free (PutStrLn s next)) = go inputs (s : outputs) next
    go (input : inputs) outputs (Free (GetLine f)) = go inputs outputs (f input)
    go [] outputs (Free (GetLine _)) = (reverse outputs, Nothing)
    go _ outputs (Free Exit) = (reverse outputs, Nothing)

-- Часть 3: Пример использования Free монады для файлового ввода-вывода
-- ----------------------------------------------------------------

-- Определение функтора для файлового ввода-вывода
data FileIOF a
  = ReadFile FilePath (String -> a)
  | WriteFile FilePath String a
  | AppendFile FilePath String a
  deriving Functor

-- Тип для программы, использующей файловый ввод-вывод
type FileIO a = Free FileIOF a

-- Умные конструкторы для файловых операций
readFile' :: FilePath -> FileIO String
readFile' path = liftF (ReadFile path id)

writeFile' :: FilePath -> String -> FileIO ()
writeFile' path content = liftF (WriteFile path content ())

appendFile' :: FilePath -> String -> FileIO ()
appendFile' path content = liftF (AppendFile path content ())

-- Пример программы, использующей файловый ввод-вывод
copyFile :: FilePath -> FilePath -> FileIO ()
copyFile src dst = do
  content <- readFile' src
  writeFile' dst content

appendFiles :: FilePath -> [FilePath] -> FileIO ()
appendFiles dst srcs = do
  contents <- mapM readFile' srcs
  writeFile' dst (concat contents)

-- Интерпретатор для файлового ввода-вывода в IO
runFileIO :: FileIO a -> IO a
runFileIO = foldFree $ \case
  ReadFile path f -> do
    content <- readFile path
    return (f content)
  WriteFile path content next -> do
    writeFile path content
    return next
  AppendFile path content next -> do
    appendFile path content
    return next

-- Интерпретатор для тестирования файлового ввода-вывода
type FileSystem = [(FilePath, String)]

runFilePure :: FileSystem -> FileIO a -> (FileSystem, a)
runFilePure fs0 = go fs0
  where
    go :: FileSystem -> Free FileIOF a -> (FileSystem, a)
    go fs (Pure a) = (fs, a)
    go fs (Free (ReadFile path f)) =
      case lookup path fs of
        Just content -> go fs (f content)
        Nothing -> error $ "File not found: " ++ path
    go fs (Free (WriteFile path content next)) =
      let fs' = updateFile fs path content
      in go fs' next
    go fs (Free (AppendFile path content next)) =
      let oldContent = maybe "" id (lookup path fs)
          fs' = updateFile fs path (oldContent ++ content)
      in go fs' next
    
    updateFile :: FileSystem -> FilePath -> String -> FileSystem
    updateFile fs path content =
      (path, content) : filter (\(p, _) -> p /= path) fs

-- Часть 4: Комбинирование Free монад
-- -------------------------------

-- Определение суммы функторов
data (f :+: g) a = InL (f a) | InR (g a)
  deriving Functor

-- Инъекции для суммы функторов
class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = id

instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f :<: (g :+: f) where
  inj = InR

-- Функция для подъема функтора в Free монаду через инъекцию
liftFree :: (f :<: g) => f a -> Free g a
liftFree = liftF . inj

-- Определение комбинированного функтора для консольного и файлового ввода-вывода
type ConsoleAndFileIO = ConsoleF :+: FileIOF

-- Умные конструкторы для комбинированного функтора
putStrLn'' :: (ConsoleF :<: f) => String -> Free f ()
putStrLn'' s = liftFree (PutStrLn s ())

getLine'' :: (ConsoleF :<: f) => Free f String
getLine'' = liftFree (GetLine id)

readFile'' :: (FileIOF :<: f) => FilePath -> Free f String
readFile'' path = liftFree (ReadFile path id)

writeFile'' :: (FileIOF :<: f) => FilePath -> String -> Free f ()
writeFile'' path content = liftFree (WriteFile path content ())

appendFile'' :: (FileIOF :<: f) => FilePath -> String -> Free f ()
appendFile'' path content = liftFree (AppendFile path content ())

-- Пример программы, использующей комбинированный функтор
catProgram :: (ConsoleF :<: f, FileIOF :<: f) => Free f ()
catProgram = do
  putStrLn'' "Введите имя файла:"
  path <- getLine''
  content <- readFile'' path
  putStrLn'' content
  putStrLn'' "Хотите сохранить содержимое в другой файл? (y/n)"
  answer <- getLine''
  if answer == "y"
    then do
      putStrLn'' "Введите имя файла для сохранения:"
      outPath <- getLine''
      writeFile'' outPath content
      putStrLn'' "Файл сохранен."
    else putStrLn'' "Выход без сохранения."

-- Интерпретатор для комбинированного функтора в IO
runCombinedIO :: Free ConsoleAndFileIO a -> IO a
runCombinedIO = foldFree $ \case
  InL (PutStrLn s next) -> do
    putStrLn s
    return next
  InL (GetLine f) -> do
    input <- getLine
    return (f input)
  InL Exit -> error "Exit"
  InR (ReadFile path f) -> do
    content <- readFile path
    return (f content)
  InR (WriteFile path content next) -> do
    writeFile path content
    return next
  InR (AppendFile path content next) -> do
    appendFile path content
    return next

-- Часть 5: Free монады и эффекты
-- ---------------------------

-- Определение типа для эффектов
data Effect a
  = ReadEffect (IO a)
  | WriteEffect String (IO a)
  | RandomEffect (Int -> a)
  deriving Functor

-- Тип для программы с эффектами
type EffectM a = Free Effect a

-- Умные конструкторы для эффектов
readEffect :: IO a -> EffectM a
readEffect io = liftF (ReadEffect io)

writeEffect :: String -> IO a -> EffectM a
writeEffect s io = liftF (WriteEffect s io)

randomEffect :: (Int -> a) -> EffectM a
randomEffect f = liftF (RandomEffect f)

-- Пример программы с эффектами
effectProgram :: EffectM ()
effectProgram = do
  writeEffect "Генерация случайного числа..." (return ())
  n <- randomEffect id
  writeEffect ("Случайное число: " ++ show n) (return ())
  if n `mod` 2 == 0
    then writeEffect "Число четное." (return ())
    else writeEffect "Число нечетное." (return ())

-- Интерпретатор для эффектов в IO
runEffectIO :: EffectM a -> IO a
runEffectIO = foldFree $ \case
  ReadEffect io -> io
  WriteEffect s io -> do
    putStrLn s
    io
  RandomEffect f -> do
    n <- randomRIO (1, 100)
    return (f n)
  where
    randomRIO :: (Int, Int) -> IO Int
    randomRIO (lo, hi) = return $ lo + (hi - lo) `div` 2  -- Упрощенная версия

-- Интерпретатор для тестирования эффектов
runEffectPure :: Int -> EffectM () -> ([String], ())
runEffectPure seed program = go seed [] program
  where
    go :: Int -> [String] -> EffectM a -> ([String], ())
    go _ outputs (Pure _) = (reverse outputs, ())
    go seed outputs (Free (ReadEffect _)) = 
      error "Cannot perform IO in pure interpreter"
    go seed outputs (Free (WriteEffect s _)) = 
      go seed (s : outputs) (Pure ())
    go seed outputs (Free (RandomEffect f)) = 
      go (seed + 1) outputs (Pure (f seed))

-- Часть 6: Оптимизация Free монад
-- ----------------------------

-- Оптимизированная версия Free монады (Church-encoded)
newtype FreeC f a = FreeC
  { runFreeC :: forall r. (a -> r) -> (f r -> r) -> r }

-- Реализация Functor для FreeC
instance Functor (FreeC f) where
  fmap f (FreeC g) = FreeC $ \pure' impure -> g (pure' . f) impure

-- Реализация Applicative для FreeC
instance Functor f => Applicative (FreeC f) where
  pure a = FreeC $ \pure' _ -> pure' a
  (<*>) = ap

-- Реализация Monad для FreeC
instance Functor f => Monad (FreeC f) where
  return = pure
  FreeC m >>= f = FreeC $ \pure' impure ->
    m (\a -> runFreeC (f a) pure' impure) impure

-- Функция для подъема функтора в FreeC монаду
liftFC :: Functor f => f a -> FreeC f a
liftFC fa = FreeC $ \pure' impure -> impure (fmap pure' fa)

-- Функция для интерпретации FreeC монады
foldFreeC :: Functor f => (a -> b) -> (f b -> b) -> FreeC f a -> b
foldFreeC pure' impure (FreeC f) = f pure' impure

-- Преобразование между Free и FreeC
fromFree :: Functor f => Free f a -> FreeC f a
fromFree (Pure a) = pure a
fromFree (Free fa) = FreeC $ \pure' impure ->
  impure (fmap (\m -> runFreeC (fromFree m) pure' impure) fa)

toFree :: Functor f => FreeC f a -> Free f a
toFree = foldFreeC Pure (Free . fmap id)

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Консольный ввод-вывод с Free монадой"
  
  putStrLn "Запуск программы приветствия:"
  runConsoleIO $ do
    putStrLn' "Это демонстрация Free монады."
    putStrLn' "Введите что-нибудь:"
    input <- getLine'
    putStrLn' $ "Вы ввели: " ++ input
  
  putStrLn "\nТестирование программы приветствия:"
  let (outputs, _) = runConsolePure ["Тестовый ввод"] $ do
        putStrLn' "Это демонстрация Free монады."
        putStrLn' "Введите что-нибудь:"
        input <- getLine'
        putStrLn' $ "Вы ввели: " ++ input
  
  putStrLn "Вывод программы:"
  mapM_ putStrLn outputs

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Файловый ввод-вывод с Free монадой"
  
  putStrLn "Тестирование программы копирования файла:"
  let initialFS = [("source.txt", "Это содержимое исходного файла.")]
      (finalFS, _) = runFilePure initialFS (copyFile "source.txt" "dest.txt")
  
  putStrLn "Начальная файловая система:"
  mapM_ (\(path, content) -> putStrLn $ path ++ ": " ++ content) initialFS
  
  putStrLn "\nКонечная файловая система:"
  mapM_ (\(path, content) -> putStrLn $ path ++ ": " ++ content) finalFS

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Комбинирование Free монад"
  
  putStrLn "Тестирование программы cat:"
  let initialFS = [("test.txt", "Это содержимое тестового файла.")]
      inputs = ["test.txt", "y", "output.txt"]
      
      program :: Free ConsoleAndFileIO ()
      program = catProgram
      
      interpreter :: Free ConsoleAndFileIO a -> ([String], FileSystem, Maybe a)
      interpreter = go inputs [] initialFS
        where
          go :: [String] -> [String] -> FileSystem -> Free ConsoleAndFileIO a -> ([String], FileSystem, Maybe a)
          go _ outputs fs (Pure a) = (reverse outputs, fs, Just a)
          go inputs outputs fs (Free (InL (PutStrLn s next))) = go inputs (s : outputs) fs next
          go (input : inputs) outputs fs (Free (InL (GetLine f))) = go inputs outputs fs (f input)
          go [] outputs fs (Free (InL (GetLine _))) = (reverse outputs, fs, Nothing)
          go _ outputs fs (Free (InL Exit)) = (reverse outputs, fs, Nothing)
          go inputs outputs fs (Free (InR (ReadFile path f))) =
            case lookup path fs of
              Just content -> go inputs outputs fs (f content)
              Nothing -> (reverse outputs, fs, Nothing)
          go inputs outputs fs (Free (InR (WriteFile path content next))) =
            let fs' = (path, content) : filter (\(p, _) -> p /= path) fs
            in go inputs outputs fs' next
          go inputs outputs fs (Free (InR (AppendFile path content next))) =
            let oldContent = maybe "" id (lookup path fs)
                fs' = (path, oldContent ++ content) : filter (\(p, _) -> p /= path) fs
            in go inputs outputs fs' next
  
  let (outputs, finalFS, _) = interpreter program
  
  putStrLn "Вывод программы:"
  mapM_ putStrLn outputs
  
  putStrLn "\nКонечная файловая система:"
  mapM_ (\(path, content) -> putStrLn $ path ++ ": " ++ content) finalFS

-- Главная функция
main :: IO ()
main = do
  putStrLn "Free монады в Haskell\n"
  
  example1
  example2
  example3
  
  putStrLn "\nКлючевые моменты о Free монадах:"
  putStrLn "1. Free монада - это способ превратить любой функтор в монаду"
  putStrLn "2. Free монады позволяют разделить описание вычисления и его интерпретацию"
  putStrLn "3. Free монады можно комбинировать с помощью суммы функторов"
  putStrLn "4. Free монады позволяют создавать предметно-ориентированные языки (DSL)"
  putStrLn "5. Free монады можно оптимизировать с помощью Church-encoding"
  putStrLn "6. Free монады широко используются в функциональном программировании для моделирования эффектов"
