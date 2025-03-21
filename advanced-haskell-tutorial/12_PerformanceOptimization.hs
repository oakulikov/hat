{-
  Оптимизация производительности в Haskell
  
  В этом файле мы рассмотрим различные техники оптимизации
  производительности Haskell-программ, включая строгость,
  структуры данных, профилирование и другие подходы.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (foldM, when, forM_, filterM)
import Control.Parallel (par, pseq)
import Control.Monad.ST
import Data.Bits
import Data.Char (ord)
import Data.IORef
import Data.STRef
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts
import GHC.IO (IO(..))
import GHC.Prim
import GHC.Types
import System.IO.Unsafe (unsafePerformIO)
import System.Mem
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM

-- Часть 1: Понимание ленивости и строгости
-- ------------------------------------

{-
Haskell - ленивый язык по умолчанию, что означает, что выражения вычисляются только
когда их значения действительно нужны. Это имеет свои преимущества, но может привести
к проблемам с производительностью, особенно к утечкам памяти из-за накопления невычисленных
выражений (thunks).

Основные техники управления строгостью:
1. Строгие функции свертки (foldl')
2. Строгие поля в типах данных (!)
3. Принудительное вычисление с помощью seq, $!, deepseq
4. Строгие версии стандартных структур данных (Map.Strict, IntMap.Strict и т.д.)
-}

-- Пример 1: Проблема с ленивым foldl
sumLazy :: [Int] -> Int
sumLazy = foldl (+) 0

-- Решение: использование строгого foldl'
sumStrict :: [Int] -> Int
sumStrict = foldl' (+) 0

-- Пример 2: Ленивые и строгие поля в типах данных
data Point = Point Int Int  -- Ленивые поля
data Point' = Point' !Int !Int  -- Строгие поля

-- Пример 3: Принудительное вычисление
forceComputation :: [Int] -> Int
forceComputation xs = let sum = foldl (+) 0 xs
                      in sum `seq` sum

-- Использование deepseq для полного вычисления структуры
forceFullComputation :: NFData a => a -> a
forceFullComputation x = deepseq x x

-- Часть 2: Эффективные структуры данных
-- ---------------------------------

{-
Выбор правильной структуры данных может значительно повлиять на производительность.
Основные эффективные структуры данных в Haskell:

1. Векторы (Data.Vector) - более эффективны, чем списки для индексированного доступа
2. Unboxed векторы - хранят примитивные типы без упаковки
3. Storable векторы - для взаимодействия с C
4. ByteString - эффективная замена String для бинарных данных
5. Text - эффективная замена String для текста
6. IntMap - специализированная карта для целочисленных ключей
7. HashMap - хеш-карта с O(1) доступом
-}

-- Пример 1: Сравнение списков и векторов
sumList :: [Int] -> Int
sumList = sum

sumVector :: V.Vector Int -> Int
sumVector = V.sum

sumUnboxedVector :: VU.Vector Int -> Int
sumUnboxedVector = VU.sum

-- Пример 2: Сравнение String и Text/ByteString
countCharsString :: String -> Int
countCharsString = length

countCharsText :: T.Text -> Int
countCharsText = T.length

countCharsByteString :: BS.ByteString -> Int
countCharsByteString = BS.length

-- Пример 3: Использование специализированных карт
lookupIntMap :: Int -> IM.IntMap a -> Maybe a
lookupIntMap = IM.lookup

-- Часть 3: Оптимизация рекурсии и хвостовая рекурсия
-- ----------------------------------------------

{-
Рекурсия - основной способ выражения итерации в Haskell, но неоптимальная
рекурсия может привести к переполнению стека или неэффективному использованию памяти.

Основные техники оптимизации рекурсии:
1. Хвостовая рекурсия - когда рекурсивный вызов является последней операцией в функции
2. Накопление результата в аккумуляторе
3. Использование строгих аккумуляторов
-}

-- Пример 1: Наивная реализация факториала (не хвостовая рекурсия)
factorialNaive :: Integer -> Integer
factorialNaive 0 = 1
factorialNaive n = n * factorialNaive (n - 1)

-- Пример 2: Хвостовая рекурсия с аккумулятором
factorialTailRec :: Integer -> Integer
factorialTailRec n = go n 1
  where
    go 0 acc = acc
    go m acc = go (m - 1) (m * acc)

-- Пример 3: Хвостовая рекурсия со строгим аккумулятором
factorialStrict :: Integer -> Integer
factorialStrict n = go n 1
  where
    go 0 !acc = acc
    go m !acc = go (m - 1) (m * acc)

-- Часть 4: Использование примитивных операций и FFI
-- ---------------------------------------------

{-
Для максимальной производительности иногда необходимо использовать
низкоуровневые примитивные операции или вызывать функции из C.

Основные техники:
1. Использование примитивных типов и операций из GHC.Prim
2. Использование FFI для вызова функций из C
3. Использование Storable для эффективного обмена данными с C
-}

-- Пример 1: Использование примитивных операций
addIntPrim :: Int -> Int -> Int
addIntPrim (I# x) (I# y) = I# (x +# y)

-- Пример 2: Использование FFI для вызова функций из C
foreign import ccall unsafe "math.h sin"
  c_sin :: CDouble -> CDouble

sinFromC :: Double -> Double
sinFromC x = realToFrac (c_sin (realToFrac x))

-- Пример 3: Использование Storable для работы с массивами
sumStorableVector :: VS.Vector Double -> Double
sumStorableVector v = VS.sum v

-- Часть 5: Параллелизм и конкурентность
-- ---------------------------------

{-
Haskell предоставляет мощные инструменты для параллельного и конкурентного программирования.

Основные техники:
1. par и pseq для явного параллелизма
2. Стратегии оценки (Evaluation Strategies)
3. Параллельные комбинаторы из Control.Parallel
4. Конкурентные потоки с помощью forkIO
5. Асинхронное программирование с помощью async
-}

-- Пример 1: Использование par и pseq
parFib :: Integer -> Integer
parFib n
  | n <= 1 = n
  | otherwise = x `par` (y `pseq` (x + y))
  where
    x = parFib (n - 1)
    y = parFib (n - 2)

-- Пример 2: Параллельная обработка списка
parMap :: (a -> b) -> [a] -> [b]
parMap f [] = []
parMap f (x:xs) = let r = f x
                      rs = parMap f xs
                  in r `par` (rs `pseq` (r : rs))

-- Часть 6: Оптимизация компилятора и флаги
-- ------------------------------------

{-
GHC предоставляет множество флагов для оптимизации кода.

Основные флаги:
1. -O, -O2 - уровни оптимизации
2. -fllvm - использование LLVM для генерации кода
3. -prof, -fprof-auto - профилирование
4. -fspec-constr - специализация конструкторов
5. -funbox-strict-fields - автоматическая распаковка строгих полей

Прагмы для оптимизации:
1. INLINE, INLINABLE - подсказки для встраивания функций
2. SPECIALIZE - специализация полиморфных функций
3. RULES - правила переписывания для оптимизации
-}

-- Пример 1: Использование INLINE
{-# INLINE square #-}
square :: Int -> Int
square x = x * x

-- Пример 2: Использование SPECIALIZE
{-# SPECIALIZE sumGeneric :: [Int] -> Int #-}
{-# SPECIALIZE sumGeneric :: [Double] -> Double #-}
sumGeneric :: Num a => [a] -> a
sumGeneric = foldl' (+) 0

-- Пример 3: Использование RULES
{-# RULES
"map/map"    forall f g xs.  map f (map g xs) = map (f . g) xs
"map/filter" forall f p xs.  map f (filter p xs) = filter (p . f) (map f xs)
  #-}

-- Часть 7: Профилирование и измерение производительности
-- ------------------------------------------------

{-
Профилирование - важный шаг в оптимизации, позволяющий выявить узкие места.

Основные инструменты:
1. Профилирование времени и памяти с помощью +RTS -p, +RTS -h
2. Визуализация с помощью hp2ps, threadscope
3. Измерение времени выполнения с помощью criterion
4. Ручное измерение с помощью getCurrentTime
-}

-- Пример: Ручное измерение времени выполнения
timeIt :: IO a -> IO (a, NominalDiffTime)
timeIt action = do
  start <- getCurrentTime
  result <- action
  end <- getCurrentTime
  return (result, diffUTCTime end start)

-- Часть 8: Управление памятью и сборка мусора
-- ---------------------------------------

{-
Эффективное управление памятью критично для производительности.

Основные техники:
1. Принудительная сборка мусора с помощью System.Mem.performGC
2. Настройка параметров сборки мусора с помощью +RTS -A, +RTS -H и т.д.
3. Компактные регионы для эффективного хранения данных
4. Использование IORef и STRef для изменяемого состояния
5. Использование mutable векторов для эффективных алгоритмов
-}

-- Пример 1: Использование IORef для изменяемого состояния
countWithIORef :: [a] -> IO Int
countWithIORef xs = do
  counter <- newIORef 0
  mapM_ (\_ -> modifyIORef' counter (+1)) xs
  readIORef counter

-- Пример 2: Использование ST монады и изменяемых векторов
sumVectorST :: VU.Vector Int -> Int
sumVectorST vec = runST $ do
  let n = VU.length vec
  result <- newSTRef 0
  forM_ [0..n-1] $ \i -> do
    let val = vec VU.! i
    modifySTRef' result (+ val)
  readSTRef result

-- Пример 3: Эффективная сортировка с использованием изменяемых векторов
sortVector :: VU.Vector Int -> VU.Vector Int
sortVector vec = VU.modify sort vec
  where
    sort :: VUM.MVector s Int -> ST s ()
    sort mv = do
      let n = VUM.length mv
      forM_ [0..n-2] $ \i ->
        forM_ [i+1..n-1] $ \j -> do
          vi <- VUM.read mv i
          vj <- VUM.read mv j
          when (vj < vi) $ do
            VUM.write mv i vj
            VUM.write mv j vi

-- Часть 9: Практические примеры оптимизации
-- -------------------------------------

-- Пример 1: Оптимизация вычисления чисел Фибоначчи
-- Наивная реализация
fibNaive :: Integer -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

-- Оптимизированная реализация с мемоизацией
fibMemo :: Integer -> Integer
fibMemo n = fibs !! (fromIntegral n)
  where
    fibs :: [Integer]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Оптимизированная итеративная реализация
fibIter :: Integer -> Integer
fibIter n = snd $ foldl' (\(a, b) _ -> (b, a + b)) (0, 1) [1..n]

-- Пример 2: Оптимизация поиска простых чисел
-- Наивная реализация
isPrimeNaive :: Integer -> Bool
isPrimeNaive n
  | n <= 1 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

-- Оптимизированная реализация с решетом Эратосфена
primesUpTo :: Int -> [Int]
primesUpTo n = runST $ do
  sieve <- VM.replicate (n + 1) True
  VM.write sieve 0 False
  VM.write sieve 1 False
  
  forM_ [2..floor (sqrt (fromIntegral n))] $ \i -> do
    isPrime <- VM.read sieve i
    when isPrime $
      forM_ [i*i, i*i+i..n] $ \j ->
        VM.write sieve j False
  
  filterM (VM.read sieve) [2..n]

-- Пример 3: Оптимизация обработки текста
-- Наивная реализация подсчета слов
countWordsNaive :: String -> Int
countWordsNaive = length . words

-- Оптимизированная реализация с использованием Text
countWordsOptimized :: T.Text -> Int
countWordsOptimized = length . T.words

-- Оптимизированная реализация с использованием ByteString
countWordsBS :: BS.ByteString -> Int
countWordsBS = length . BSC.words

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Строгость и ленивость"
  
  let bigList = [1..1000000]
  
  putStrLn "Вычисление суммы с ленивым foldl..."
  (_, time1) <- timeIt $ evaluate $ sumLazy bigList
  putStrLn $ "Время: " ++ show time1
  
  putStrLn "Вычисление суммы со строгим foldl'..."
  (_, time2) <- timeIt $ evaluate $ sumStrict bigList
  putStrLn $ "Время: " ++ show time2
  
  putStrLn $ "Соотношение: " ++ show (time1 / time2)

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Эффективные структуры данных"
  
  let n = 1000000
  let list = [1..n]
  let vector = V.fromList list
  let unboxedVector = VU.fromList list
  
  putStrLn "Сумма списка..."
  (_, time1) <- timeIt $ evaluate $ sumList list
  putStrLn $ "Время: " ++ show time1
  
  putStrLn "Сумма вектора..."
  (_, time2) <- timeIt $ evaluate $ sumVector vector
  putStrLn $ "Время: " ++ show time2
  
  putStrLn "Сумма unboxed вектора..."
  (_, time3) <- timeIt $ evaluate $ sumUnboxedVector unboxedVector
  putStrLn $ "Время: " ++ show time3
  
  putStrLn $ "Соотношение список/вектор: " ++ show (time1 / time2)
  putStrLn $ "Соотношение список/unboxed: " ++ show (time1 / time3)

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Оптимизация рекурсии"
  
  let n = 20
  
  putStrLn "Факториал с наивной рекурсией..."
  (_, time1) <- timeIt $ evaluate $ factorialNaive n
  putStrLn $ "Время: " ++ show time1
  
  putStrLn "Факториал с хвостовой рекурсией..."
  (_, time2) <- timeIt $ evaluate $ factorialTailRec n
  putStrLn $ "Время: " ++ show time2
  
  putStrLn "Факториал со строгим аккумулятором..."
  (_, time3) <- timeIt $ evaluate $ factorialStrict n
  putStrLn $ "Время: " ++ show time3
  
  putStrLn $ "Соотношение наивный/хвостовой: " ++ show (time1 / time2)
  putStrLn $ "Соотношение наивный/строгий: " ++ show (time1 / time3)

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Практические примеры оптимизации"
  
  let n = 30
  
  putStrLn "Числа Фибоначчи с наивной рекурсией..."
  (_, time1) <- timeIt $ evaluate $ fibNaive n
  putStrLn $ "Время: " ++ show time1
  
  putStrLn "Числа Фибоначчи с мемоизацией..."
  (_, time2) <- timeIt $ evaluate $ fibMemo n
  putStrLn $ "Время: " ++ show time2
  
  putStrLn "Числа Фибоначчи с итеративным подходом..."
  (_, time3) <- timeIt $ evaluate $ fibIter n
  putStrLn $ "Время: " ++ show time3
  
  putStrLn $ "Соотношение наивный/мемоизация: " ++ show (time1 / time2)
  putStrLn $ "Соотношение наивный/итеративный: " ++ show (time1 / time3)
  
  let text = replicate 1000000 'a'
  let textT = T.pack text
  let textBS = BSC.pack text
  
  putStrLn "\nПодсчет слов в String..."
  (_, time4) <- timeIt $ evaluate $ countWordsNaive text
  putStrLn $ "Время: " ++ show time4
  
  putStrLn "Подсчет слов в Text..."
  (_, time5) <- timeIt $ evaluate $ countWordsOptimized textT
  putStrLn $ "Время: " ++ show time5
  
  putStrLn "Подсчет слов в ByteString..."
  (_, time6) <- timeIt $ evaluate $ countWordsBS textBS
  putStrLn $ "Время: " ++ show time6
  
  putStrLn $ "Соотношение String/Text: " ++ show (time4 / time5)
  putStrLn $ "Соотношение String/ByteString: " ++ show (time4 / time6)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Оптимизация производительности в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты об оптимизации производительности:"
  putStrLn "1. Управление строгостью критично для предотвращения утечек памяти"
  putStrLn "2. Выбор правильных структур данных может значительно улучшить производительность"
  putStrLn "3. Хвостовая рекурсия и строгие аккумуляторы помогают оптимизировать рекурсивные функции"
  putStrLn "4. Для максимальной производительности можно использовать примитивные операции и FFI"
  putStrLn "5. Параллелизм и конкурентность позволяют использовать многоядерные процессоры"
  putStrLn "6. Флаги компилятора и прагмы помогают GHC оптимизировать код"
  putStrLn "7. Профилирование помогает выявить узкие места в производительности"
  putStrLn "8. Эффективное управление памятью и сборкой мусора важно для долго работающих программ"
