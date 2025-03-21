{-
  Продвинутые функторы в Haskell
  
  В этом файле мы рассмотрим расширенные концепции функторов,
  включая бифункторы, профункторы и другие вариации функторов.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Bifunctor
import Data.Profunctor
import Data.Functor.Contravariant
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Functor.Const
import Control.Arrow
import Control.Applicative

-- Часть 1: Обзор стандартных функторов
-- ---------------------------------

{-
Функтор - это тип, который поддерживает операцию fmap, позволяющую применять функции
к значениям внутри контекста. Формально, функтор - это тип f, для которого определена
функция:

fmap :: (a -> b) -> f a -> f b

Функтор должен удовлетворять следующим законам:
1. fmap id = id (сохранение идентичности)
2. fmap (g . h) = fmap g . fmap h (сохранение композиции)

В этом файле мы рассмотрим различные вариации и обобщения функторов.
-}

-- Примеры стандартных функторов
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Functor)
data Maybe' a = Nothing' | Just' a deriving (Show, Eq, Functor)
data Either' a b = Left' a | Right' b deriving (Show, Eq, Functor)
newtype Identity' a = Identity' a deriving (Show, Eq, Functor)
newtype Const' a b = Const' a deriving (Show, Eq, Functor)

-- Часть 2: Бифункторы
-- ----------------

{-
Бифунктор - это тип с двумя параметрами, к которым можно применять функции независимо.
Формально, бифунктор - это тип f, для которого определена функция:

bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

Бифунктор должен удовлетворять следующим законам:
1. bimap id id = id (сохранение идентичности)
2. bimap (f . g) (h . i) = bimap f h . bimap g i (сохранение композиции)

Бифунктор также предоставляет функции first и second для применения функции только к первому
или только ко второму параметру:

first :: (a -> c) -> f a b -> f c b
second :: (b -> d) -> f a b -> f a d
-}

-- Примеры бифункторов
instance Bifunctor Either' where
  bimap f _ (Left' a) = Left' (f a)
  bimap _ g (Right' b) = Right' (g b)
  
  first f (Left' a) = Left' (f a)
  first _ (Right' b) = Right' b
  
  second _ (Left' a) = Left' a
  second g (Right' b) = Right' (g b)

-- Пара как бифунктор
instance Bifunctor (,) where
  bimap f g (a, b) = (f a, g b)
  first f (a, b) = (f a, b)
  second g (a, b) = (a, g b)

-- Собственный бифунктор
data These a b = This a | That b | These' a b deriving (Show, Eq)

instance Bifunctor These where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)
  bimap f g (These' a b) = These' (f a) (g b)
  
  first f (This a) = This (f a)
  first f (These' a b) = These' (f a) b
  first _ (That b) = That b
  
  second g (That b) = That (g b)
  second g (These' a b) = These' a (g b)
  second _ (This a) = This a

-- Часть 3: Контравариантные функторы
-- -------------------------------

{-
Контравариантный функтор - это тип, который позволяет применять функции "в обратном направлении".
Формально, контравариантный функтор - это тип f, для которого определена функция:

contramap :: (b -> a) -> f a -> f b

Контравариантный функтор должен удовлетворять следующим законам:
1. contramap id = id (сохранение идентичности)
2. contramap (f . g) = contramap g . contramap f (обратное сохранение композиции)

Обратите внимание на порядок композиции в законе 2: он обратный по сравнению с обычным функтором.
-}

-- Примеры контравариантных функторов
newtype Predicate a = Predicate { runPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)

-- Пример использования Predicate
isEven :: Predicate Int
isEven = Predicate (\x -> x `mod` 2 == 0)

isEvenLength :: Predicate String
isEvenLength = contramap length isEven

-- Компаратор как контравариантный функтор
newtype Comparison a = Comparison { runComparison :: a -> a -> Ordering }

instance Contravariant Comparison where
  contramap f (Comparison comp) = Comparison (\x y -> comp (f x) (f y))

-- Пример использования Comparison
byLength :: Comparison String
byLength = Comparison (\x y -> length x `compare` length y)

byFirstChar :: Comparison String
byFirstChar = Comparison (\x y -> head x `compare` head y)

-- Комбинирование компараторов
thenCompare :: Comparison a -> Comparison a -> Comparison a
thenCompare (Comparison c1) (Comparison c2) = Comparison $ \x y ->
  case c1 x y of
    EQ -> c2 x y
    result -> result

-- Часть 4: Профункторы
-- -----------------

{-
Профунктор - это тип с двумя параметрами, который контравариантен по первому параметру
и ковариантен по второму. Формально, профунктор - это тип p, для которого определена функция:

dimap :: (a -> b) -> (c -> d) -> p b c -> p a d

Профунктор должен удовлетворять следующим законам:
1. dimap id id = id (сохранение идентичности)
2. dimap (f . g) (h . i) = dimap g i . dimap f h (смешанное сохранение композиции)

Профунктор также предоставляет функции lmap и rmap для применения функции только к первому
или только ко второму параметру:

lmap :: (a -> b) -> p b c -> p a c
rmap :: (b -> c) -> p a b -> p a c
-}

-- Примеры профункторов
instance Profunctor (->) where
  dimap f g h = g . h . f
  lmap f g = g . f
  rmap = (.)

-- Kleisli стрелки как профунктор
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Profunctor (Kleisli m) where
  dimap f g (Kleisli h) = Kleisli (fmap g . h . f)
  lmap f (Kleisli h) = Kleisli (h . f)
  rmap g (Kleisli h) = Kleisli (fmap g . h)

-- Cokleisli стрелки как профунктор
newtype Cokleisli w a b = Cokleisli { runCokleisli :: w a -> b }

instance Functor w => Profunctor (Cokleisli w) where
  dimap f g (Cokleisli h) = Cokleisli (g . h . fmap f)
  lmap f (Cokleisli h) = Cokleisli (h . fmap f)
  rmap g (Cokleisli h) = Cokleisli (g . h)

-- Часть 5: Инвариантные функторы
-- ---------------------------

{-
Инвариантный функтор - это тип, который позволяет применять функции в обоих направлениях.
Формально, инвариантный функтор - это тип f, для которого определена функция:

imap :: (a -> b) -> (b -> a) -> f a -> f b

Инвариантный функтор должен удовлетворять следующим законам:
1. imap id id = id (сохранение идентичности)
2. imap f1 g1 . imap f2 g2 = imap (f1 . f2) (g2 . g1) (сохранение композиции)
-}

class Invariant f where
  imap :: (a -> b) -> (b -> a) -> f a -> f b

-- Примеры инвариантных функторов
newtype Endo a = Endo { appEndo :: a -> a }

instance Invariant Endo where
  imap f g (Endo h) = Endo (f . h . g)

-- Пример использования Endo
addOne :: Endo Int
addOne = Endo (+ 1)

addOneToString :: Endo String
addOneToString = imap show read addOne

-- Часть 6: Композиция функторов
-- --------------------------

{-
Функторы можно комбинировать различными способами, создавая новые функторы.
Наиболее распространенные комбинации:

1. Композиция функторов: Compose f g a = f (g a)
2. Произведение функторов: Product f g a = (f a, g a)
3. Сумма функторов: Sum f g a = Left (f a) | Right (g a)
-}

-- Примеры композиции функторов
type MaybeList a = Compose Maybe [] a
type ListMaybe a = Compose [] Maybe a

-- Создание значений композиции функторов
maybeListExample :: MaybeList Int
maybeListExample = Compose (Just [1, 2, 3])

listMaybeExample :: ListMaybe Int
listMaybeExample = Compose [Just 1, Nothing, Just 3]

-- Примеры произведения функторов
type MaybeAndList a = Product Maybe [] a

-- Создание значений произведения функторов
maybeAndListExample :: MaybeAndList Int
maybeAndListExample = Pair (Just 1) [2, 3, 4]

-- Примеры суммы функторов
type MaybeOrList a = Sum Maybe [] a

-- Создание значений суммы функторов
maybeOrListExample1 :: MaybeOrList Int
maybeOrListExample1 = InL (Just 1)

maybeOrListExample2 :: MaybeOrList Int
maybeOrListExample2 = InR [1, 2, 3]

-- Часть 7: Свободные функторы
-- ------------------------

{-
Свободный функтор - это функтор, который не имеет дополнительной структуры кроме
той, что требуется для удовлетворения законов функтора. Свободный функтор для типа f
можно представить как:

data Free f a = Pure a | Free (f (Free f a))

Свободный функтор позволяет "поднять" любой тип до функтора.
-}

-- Определение свободного функтора
data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

-- Пример использования свободного функтора
data Console a = GetLine (String -> a) | PutLine String a

instance Functor Console where
  fmap f (GetLine g) = GetLine (f . g)
  fmap f (PutLine s a) = PutLine s (f a)

type ConsoleProgram a = Free Console a

-- Создание программы с использованием свободного функтора
getLine' :: ConsoleProgram String
getLine' = Free (GetLine Pure)

putLine' :: String -> ConsoleProgram ()
putLine' s = Free (PutLine s (Pure ()))

-- Часть 8: Прикладные примеры использования продвинутых функторов
-- ----------------------------------------------------------

-- Пример 1: Обработка данных с использованием бифункторов
processData :: Bifunctor f => (a -> c) -> (b -> d) -> f a b -> f c d
processData = bimap

-- Пример использования processData
processEither :: Either String Int -> Either String Double
processEither = processData id fromIntegral

processPair :: (String, Int) -> (String, Double)
processPair = processData id fromIntegral

-- Пример 2: Валидация данных с использованием контравариантных функторов
validate :: Contravariant f => (b -> a) -> f a -> f b
validate = contramap

-- Пример использования validate
validateUser :: Predicate String
validateUser = Predicate (\name -> not (null name) && length name <= 50)

validateUserEmail :: Predicate (String, String)
validateUserEmail = validate snd validateUser

-- Пример 3: Трансформация функций с использованием профункторов
transform :: Profunctor p => (a -> b) -> (c -> d) -> p b c -> p a d
transform = dimap

-- Пример использования transform
parseAndFormat :: String -> String
parseAndFormat = transform read show ((*2) :: Int -> Int)

-- Пример 4: Комбинирование функторов для создания сложных структур данных
type ComplexData a = Compose [] (Compose Maybe (Either String)) a

-- Создание значения ComplexData
complexDataExample :: ComplexData Int
complexDataExample = Compose [
  Compose (Just (Right 1)),
  Compose Nothing,
  Compose (Just (Left "Error")),
  Compose (Just (Right 3))
  ]

-- Обработка ComplexData
processComplexData :: ComplexData Int -> [String]
processComplexData (Compose xs) = concatMap processItem xs
  where
    processItem (Compose Nothing) = ["No data"]
    processItem (Compose (Just (Left err))) = ["Error: " ++ err]
    processItem (Compose (Just (Right n))) = ["Value: " ++ show n]

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Бифункторы"
  
  let pair = (1, "hello")
  let processedPair = bimap show length pair
  putStrLn $ "bimap show length (1, \"hello\") = " ++ show processedPair
  
  let these = These' 1 "hello"
  let processedThese = bimap show length these
  putStrLn $ "bimap show length (These' 1 \"hello\") = " ++ show processedThese
  
  let either = Right' "hello" :: Either' Int String
  let processedEither = bimap show length either
  putStrLn $ "bimap show length (Right' \"hello\") = " ++ show processedEither

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Контравариантные функторы"
  
  putStrLn $ "isEven 4 = " ++ show (runPredicate isEven 4)
  putStrLn $ "isEven 5 = " ++ show (runPredicate isEven 5)
  
  putStrLn $ "isEvenLength \"hello\" = " ++ show (runPredicate isEvenLength "hello")
  putStrLn $ "isEvenLength \"hi\" = " ++ show (runPredicate isEvenLength "hi")
  
  let compareByLengthThenFirstChar = thenCompare byLength byFirstChar
  putStrLn $ "compare \"abc\" \"def\" by length = " ++ show (runComparison byLength "abc" "def")
  putStrLn $ "compare \"abc\" \"def\" by first char = " ++ show (runComparison byFirstChar "abc" "def")
  putStrLn $ "compare \"abc\" \"def\" by length then first char = " ++ show (runComparison compareByLengthThenFirstChar "abc" "def")
  putStrLn $ "compare \"abc\" \"ab\" by length then first char = " ++ show (runComparison compareByLengthThenFirstChar "abc" "ab")

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Профункторы"
  
  let f = dimap (+1) (*2) (*)
  putStrLn $ "dimap (+1) (*2) (*) applied to (5, 6) = " ++ show (f 5 6)
  
  let kleisliFunc = Kleisli $ \x -> if x > 0 then Just (x * 2) else Nothing
  let dimappedKleisli = dimap (+1) show kleisliFunc
  
  putStrLn $ "kleisliFunc applied to 5 = " ++ show (runKleisli kleisliFunc 5)
  putStrLn $ "kleisliFunc applied to -1 = " ++ show (runKleisli kleisliFunc (-1))
  putStrLn $ "dimappedKleisli applied to 5 = " ++ show (runKleisli dimappedKleisli 5)
  putStrLn $ "dimappedKleisli applied to -2 = " ++ show (runKleisli dimappedKleisli (-2))

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Композиция функторов"
  
  putStrLn $ "maybeListExample = " ++ show (getCompose maybeListExample)
  putStrLn $ "listMaybeExample = " ++ show (getCompose listMaybeExample)
  
  putStrLn $ "maybeAndListExample = " ++ show maybeAndListExample
  
  putStrLn $ "maybeOrListExample1 = " ++ show maybeOrListExample1
  putStrLn $ "maybeOrListExample2 = " ++ show maybeOrListExample2
  
  putStrLn $ "processComplexData complexDataExample = " ++ show (processComplexData complexDataExample)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Продвинутые функторы в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о продвинутых функторах:"
  putStrLn "1. Бифункторы позволяют применять функции к двум параметрам типа независимо"
  putStrLn "2. Контравариантные функторы позволяют применять функции в обратном направлении"
  putStrLn "3. Профункторы комбинируют ковариантность и контравариантность"
  putStrLn "4. Инвариантные функторы требуют функций в обоих направлениях"
  putStrLn "5. Функторы можно комбинировать с помощью композиции, произведения и суммы"
  putStrLn "6. Свободные функторы позволяют поднять любой тип до функтора"
  putStrLn "7. Продвинутые функторы имеют множество практических применений в функциональном программировании"
