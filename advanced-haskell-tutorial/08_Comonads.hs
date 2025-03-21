{-
  Коммонады в Haskell
  
  В этом файле мы рассмотрим коммонады - двойственность к монадам
  и их практическое применение в функциональном программировании.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Arrow ((>>>))
import Data.Functor.Identity
import Data.List
import Data.Function (fix)
import Data.Monoid

-- Реализация простой Map для замены Data.Map
data SimpleMap k v = SimpleMap [(k, v)]

emptyMap :: SimpleMap k v
emptyMap = SimpleMap []

lookupMap :: Eq k => k -> SimpleMap k v -> Maybe v
lookupMap k (SimpleMap kvs) = lookup k kvs

insertMap :: Eq k => k -> v -> SimpleMap k v -> SimpleMap k v
insertMap k v (SimpleMap kvs) = SimpleMap $ (k, v) : filter (\(k', _) -> k' /= k) kvs

singletonMap :: k -> v -> SimpleMap k v
singletonMap k v = SimpleMap [(k, v)]

-- Часть 1: Введение в коммонады
-- --------------------------

{-
Коммонада - это двойственность к монаде. Если монада позволяет внедрять значения в контекст
и последовательно комбинировать вычисления с контекстом, то коммонада позволяет извлекать
значения из контекста и распространять вычисления с контекстом.

Класс типов Comonad определен следующим образом:
-}

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  
  -- Реализации по умолчанию
  duplicate = extend id
  extend f = fmap f . duplicate

{-
Законы коммонад:
1. extend extract = id
   (извлечение после расширения не меняет значение)
2. extract . extend f = f
   (извлечение из расширенного значения эквивалентно применению функции)
3. extend f . extend g = extend (f . extend g)
   (расширение можно комбинировать)

Коммонады полезны для:
- Обработки контекстно-зависимых вычислений
- Моделирования клеточных автоматов и других пространственных структур
- Реализации инкрементальных вычислений
- Работы с бесконечными структурами данных
-}

-- Часть 2: Примеры коммонад
-- ----------------------

-- Пример 1: Identity коммонада
instance Comonad Identity where
  extract (Identity a) = a
  duplicate w = Identity w
  extend f w = Identity (f w)

-- Пример 2: Stream коммонада (бесконечный список)
data Stream a = Cons a (Stream a) deriving Functor

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate s@(Cons _ as) = Cons s (duplicate as)
  extend f s@(Cons _ as) = Cons (f s) (extend f as)

-- Создание Stream из начального значения и функции
unfoldStream :: (a -> a) -> a -> Stream a
unfoldStream f a = Cons a (unfoldStream f (f a))

-- Создание Stream из списка (циклически)
fromList :: [a] -> Stream a
fromList [] = error "Cannot create Stream from empty list"
fromList xs = go xs
  where
    go [] = go xs
    go (y:ys) = Cons y (go ys)

-- Преобразование Stream в список (берем первые n элементов)
toList :: Int -> Stream a -> [a]
toList 0 _ = []
toList n (Cons a as) = a : toList (n - 1) as

-- Пример 3: Store коммонада (функция с сохраненным аргументом)
data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (\s' -> Store f s') s
  extend g (Store f s) = Store (\s' -> g (Store f s')) s

-- Создание Store
store :: (s -> a) -> s -> Store s a
store = Store

-- Получение функции из Store
peek :: Store s a -> (s -> a)
peek (Store f _) = f

-- Получение сохраненного значения из Store
pos :: Store s a -> s
pos (Store _ s) = s

-- Изменение сохраненного значения
seek :: s -> Store s a -> Store s a
seek s (Store f _) = Store f s

-- Изменение сохраненного значения с помощью функции
seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s) = Store f (g s)

-- Пример 4: Env коммонада (значение с окружением)
data Env e a = Env e a deriving Functor

instance Comonad (Env e) where
  extract (Env _ a) = a
  duplicate (Env e a) = Env e (Env e a)
  extend f (Env e a) = Env e (f (Env e a))

-- Создание Env
env :: e -> a -> Env e a
env = Env

-- Получение окружения из Env
ask :: Env e a -> e
ask (Env e _) = e

-- Изменение окружения
local :: (e -> e) -> Env e a -> Env e a
local f (Env e a) = Env (f e) a

-- Пример 5: Traced коммонада (функция с моноидом)
newtype Traced m a = Traced { runTraced :: m -> a } deriving Functor

instance Monoid m => Comonad (Traced m) where
  extract (Traced f) = f mempty
  duplicate (Traced f) = Traced $ \m -> Traced $ \m' -> f (m `mappend` m')
  extend g (Traced f) = Traced $ \m -> g (Traced $ \m' -> f (m `mappend` m'))

-- Создание Traced
traced :: (m -> a) -> Traced m a
traced = Traced

-- Получение значения для заданного моноида
trace :: m -> Traced m a -> a
trace m (Traced f) = f m

-- Пример 6: Zipper коммонада (список с фокусом)
data Zipper a = Zipper [a] a [a] deriving Functor

instance Comonad Zipper where
  extract (Zipper _ a _) = a
  duplicate z@(Zipper ls a rs) = Zipper (tail $ iterate moveLeft z) z (tail $ iterate moveRight z)
  extend f z@(Zipper ls a rs) = Zipper (map f $ tail $ iterate moveLeft z) (f z) (map f $ tail $ iterate moveRight z)

-- Создание Zipper из списка (фокус на первом элементе)
fromListZ :: [a] -> Maybe (Zipper a)
fromListZ [] = Nothing
fromListZ (x:xs) = Just (Zipper [] x xs)

-- Перемещение фокуса влево
moveLeft :: Zipper a -> Zipper a
moveLeft (Zipper (l:ls) a rs) = Zipper ls l (a:rs)
moveLeft z = z  -- Если нельзя переместиться влево, остаемся на месте

-- Перемещение фокуса вправо
moveRight :: Zipper a -> Zipper a
moveRight (Zipper ls a (r:rs)) = Zipper (a:ls) r rs
moveRight z = z  -- Если нельзя переместиться вправо, остаемся на месте

-- Часть 3: Применение коммонад
-- -------------------------

-- Пример 1: Клеточный автомат "Игра жизни" с использованием Store коммонады
type Grid a = Store (Int, Int) a
type Cell = Bool

-- Создание сетки из списка списков
gridFromList :: [[a]] -> Grid a
gridFromList grid = Store lookup (0, 0)
  where
    lookup (x, y)
      | x >= 0 && y >= 0 && y < length grid && x < length (grid !! y) = (grid !! y) !! x
      | otherwise = (grid !! (y `mod` length grid)) !! (x `mod` length (grid !! (y `mod` length grid)))

-- Получение соседей клетки
neighbors :: Grid Cell -> [Cell]
neighbors g = [peek g (x, y) | x <- [x0-1..x0+1], y <- [y0-1..y0+1], (x, y) /= (x0, y0)]
  where (x0, y0) = pos g

-- Правила игры "Жизнь"
gameOfLife :: Grid Cell -> Cell
gameOfLife g =
  let alive = extract g
      aliveNeighbors = length $ filter id $ neighbors g
  in if alive
     then aliveNeighbors == 2 || aliveNeighbors == 3
     else aliveNeighbors == 3

-- Один шаг игры "Жизнь"
stepGame :: Grid Cell -> Grid Cell
stepGame = extend gameOfLife

-- Пример 2: Скользящее окно с использованием Zipper коммонады
-- Вычисление скользящего среднего
movingAverage :: Int -> Zipper Double -> Double
movingAverage n z@(Zipper ls a rs) =
  let window = take n ls ++ [a] ++ take n rs
  in sum window / fromIntegral (length window)

-- Преобразование Zipper в список
toListZ :: Int -> Zipper a -> [a]
toListZ 0 _ = []
toListZ n z@(Zipper _ a _) = a : toListZ (n - 1) (moveRight z)

-- Применение скользящего среднего ко всему списку
smoothList :: Int -> [Double] -> Maybe [Double]
smoothList n xs = case fromListZ xs of
  Nothing -> Nothing
  Just z -> Just $ toListZ (length xs) $ extend (movingAverage n) z

-- Пример 3: Инкрементальные вычисления с использованием Store коммонады
-- Мемоизация функции с помощью Store
memoize :: Eq a => (a -> b) -> a -> Store (SimpleMap a b) b
memoize f a = Store lookup (singletonMap a (f a))
  where
    lookup cache = case lookupMap a cache of
      Just b -> b
      Nothing -> let b = f a in b

-- Вычисление чисел Фибоначчи с мемоизацией
fibMemo :: Integer -> Integer
fibMemo = fix $ \f n -> case n of
  0 -> 0
  1 -> 1
  _ -> f (n - 1) + f (n - 2)

-- Пример 4: Зависимые от контекста вычисления с использованием Env коммонады
-- Функция, которая использует окружение для вычисления результата
withEnv :: (e -> a -> b) -> Env e a -> b
withEnv f = extend (\w -> f (ask w) (extract w)) >>> extract

-- Пример 5: Обратное распространение с использованием Traced коммонады
-- Функция, которая вычисляет результат на основе истории
withHistory :: Monoid m => ((m -> a) -> a -> b) -> Traced m a -> b
withHistory f = extend (\w -> f (runTraced w) (extract w)) >>> extract

-- Часть 4: Связь между монадами и коммонадами
-- ---------------------------------------

{-
Монады и коммонады являются двойственными понятиями в теории категорий:

Монада:
- return :: a -> m a (внедрение значения в контекст)
- join :: m (m a) -> m a (объединение вложенных контекстов)
- bind :: (a -> m b) -> m a -> m b (последовательное применение функций с контекстом)

Коммонада:
- extract :: w a -> a (извлечение значения из контекста)
- duplicate :: w a -> w (w a) (дублирование контекста)
- extend :: (w a -> b) -> w a -> w b (распространение функций с контекстом)

Двойственность проявляется в том, что стрелки в типах направлены в противоположные стороны.
-}

-- Пример: Adjunction (сопряжение) между монадой и коммонадой
class (Functor f, Functor g) => Adjunction f g where
  unit :: a -> g (f a)
  counit :: f (g a) -> a
  leftAdjunct :: (f a -> b) -> a -> g b
  rightAdjunct :: (a -> g b) -> f a -> b

-- Примечание: полная реализация монады и коммонады из сопряжения требует более сложной типовой магии,
-- которая выходит за рамки данного примера. Здесь мы просто определяем интерфейс Adjunction.

-- Часть 5: Продвинутые примеры использования коммонад
-- ----------------------------------------------

-- Пример 1: Дифференциальное исчисление с использованием Stream коммонады
-- Вычисление производной функции
derivative :: Fractional a => Stream a -> Stream a
derivative (Cons a (Cons b rest)) = Cons (b - a) (derivative (Cons b rest))

-- Вычисление интеграла функции
integral :: Fractional a => a -> Stream a -> Stream a
integral acc (Cons a as) = Cons acc (integral (acc + a) as)

-- Пример 2: Обработка изображений с использованием Store коммонады
-- Тип для представления изображения
type Image a = Store (Int, Int) a

-- Применение фильтра к изображению
applyFilter :: ((Int, Int) -> Image a -> b) -> Image a -> Image b
applyFilter f img = extend (\i -> f (pos i) i) img

-- Фильтр размытия
blurFilter :: Fractional a => (Int, Int) -> Image a -> a
blurFilter (x, y) img =
  let neighbors = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1]]
      values = map (peek img) neighbors
  in sum values / fromIntegral (length values)

-- Фильтр обнаружения краев
edgeFilter :: (Floating a, Ord a) => (Int, Int) -> Image a -> a
edgeFilter (x, y) img =
  let horizontal = peek img (x+1, y) - peek img (x-1, y)
      vertical = peek img (x, y+1) - peek img (x, y-1)
  in sqrt (horizontal * horizontal + vertical * vertical)

-- Пример 3: Зависимые от контекста вычисления с использованием Env коммонады
-- Функция для вычисления значения с учетом окружения
computeWithEnv :: Env Int Int -> Int
computeWithEnv env = extract env * ask env

-- Применение функции ко всем значениям с учетом окружения
mapWithEnv :: (Env e a -> b) -> e -> [a] -> [b]
mapWithEnv f e = map (f . Env e)

-- Пример 4: Обработка сигналов с использованием Zipper коммонады
-- Фильтр нижних частот
lowPassFilter :: Int -> Zipper Double -> Double
lowPassFilter n z =
  let window = take n (lefts z) ++ [extract z] ++ take n (rights z)
  in sum window / fromIntegral (length window)
  where
    lefts (Zipper ls _ _) = ls
    rights (Zipper _ _ rs) = rs

-- Фильтр верхних частот
highPassFilter :: Int -> Zipper Double -> Double
highPassFilter n z =
  let avg = lowPassFilter n z
  in extract z - avg

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Stream коммонада"
  
  let s = fromList [1..5]
  putStrLn $ "Stream из [1..5]: " ++ show (toList 10 s)
  
  let doubled = fmap (*2) s
  putStrLn $ "Удвоенный Stream: " ++ show (toList 10 doubled)
  
  let naturals = unfoldStream (+1) 1
  putStrLn $ "Натуральные числа: " ++ show (toList 10 naturals)
  
  let fibs = unfoldStream (\(a, b) -> (b, a + b)) (0, 1)
  putStrLn $ "Числа Фибоначчи: " ++ show (toList 10 $ fmap fst fibs)

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Store коммонада"
  
  let squareStore = store (\x -> x * x) 5
  putStrLn $ "extract squareStore: " ++ show (extract squareStore)
  
  let movedStore = seek 10 squareStore
  putStrLn $ "extract (seek 10 squareStore): " ++ show (extract movedStore)
  
  let incrementedStore = seeks (+1) squareStore
  putStrLn $ "extract (seeks (+1) squareStore): " ++ show (extract incrementedStore)
  
  let grid = gridFromList [[False, True, False], [True, True, True], [False, True, False]]
  putStrLn $ "Клетка в центре: " ++ show (extract grid)
  
  let nextGen = stepGame grid
  putStrLn $ "Клетка в центре после одного шага: " ++ show (extract nextGen)

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Zipper коммонада"
  
  let z = fromListZ [1..10]
  case z of
    Nothing -> putStrLn "Пустой список"
    Just zipper -> do
      putStrLn $ "extract zipper: " ++ show (extract zipper)
      
      let rightZ = moveRight zipper
      putStrLn $ "extract (moveRight zipper): " ++ show (extract rightZ)
      
      let leftZ = moveLeft zipper
      putStrLn $ "extract (moveLeft zipper): " ++ show (extract leftZ)
      
      let smoothed = extend (movingAverage 2) zipper
      putStrLn $ "Сглаженный список (окно 2): " ++ show (toListZ 10 smoothed)

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Env коммонада"
  
  let e = env 10 5
  putStrLn $ "extract e: " ++ show (extract e)
  putStrLn $ "ask e: " ++ show (ask e)
  
  let multiplied = withEnv (*) e
  putStrLn $ "withEnv (*) e: " ++ show multiplied
  
  let localE = local (+5) e
  putStrLn $ "extract (local (+5) e): " ++ show (extract localE)
  putStrLn $ "ask (local (+5) e): " ++ show (ask localE)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Коммонады в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о коммонадах:"
  putStrLn "1. Коммонада - это двойственность к монаде, позволяющая извлекать значения из контекста"
  putStrLn "2. Коммонады определяются тремя операциями: extract, duplicate и extend"
  putStrLn "3. Коммонады полезны для контекстно-зависимых вычислений, таких как клеточные автоматы"
  putStrLn "4. Коммонады могут использоваться для инкрементальных вычислений и обработки сигналов"
  putStrLn "5. Существует связь между монадами и коммонадами через сопряжение (adjunction)"
  putStrLn "6. Коммонады особенно полезны для работы с пространственными и временными структурами данных"
