





{-
  Продвинутые классы типов в Haskell
  
  В этом файле мы рассмотрим продвинутые классы типов в Haskell,
  такие как Profunctor, Contravariant, Bifunctor и другие.
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Functor.Contravariant hiding (Comparison, Predicate, Op)
import Data.Functor.Const
import Data.Functor.Identity
import Data.Bifunctor
import Data.Void
import Data.Monoid (Endo(..))
import Data.Foldable (toList)

-- Часть 1: Обзор стандартной иерархии классов типов
-- -----------------------------------------------

{-
Стандартная иерархия классов типов в Haskell:

1. Functor - типы, к которым можно применить функцию
   - fmap :: (a -> b) -> f a -> f b

2. Applicative - функторы, которые поддерживают применение функций в контексте
   - pure :: a -> f a
   - (<*>) :: f (a -> b) -> f a -> f b

3. Monad - аппликативные функторы, которые поддерживают последовательное выполнение
   - return :: a -> m a
   - (>>=) :: m a -> (a -> m b) -> m b

4. Foldable - типы, которые можно свернуть
   - foldr :: (a -> b -> b) -> b -> t a -> b

5. Traversable - типы, которые можно обойти с эффектами
   - traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

Эти классы типов хорошо известны и широко используются.
В этом файле мы рассмотрим менее известные, но не менее полезные классы типов.
-}

-- Часть 2: Contravariant Functor
-- ----------------------------

{-
Contravariant Functor - это тип, который позволяет применять функции "в обратном направлении".
Если Functor позволяет применять функцию (a -> b) к f a, получая f b,
то Contravariant позволяет применять функцию (b -> a) к f a, получая f b.

class Contravariant f where
  contramap :: (b -> a) -> f a -> f b
-}

-- Пример 1: Predicate - предикат, который проверяет значение типа a
newtype Predicate a = Predicate { runPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)

-- Пример использования Predicate
isEven :: Predicate Int
isEven = Predicate (\x -> x `mod` 2 == 0)

isEvenLength :: Predicate String
isEvenLength = contramap length isEven

-- Пример 2: Comparison - сравнение значений типа a
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

-- Пример 3: Op - операция, которая принимает a и возвращает r
newtype Op r a = Op { runOp :: a -> r }

instance Contravariant (Op r) where
  contramap f (Op g) = Op (g . f)

-- Часть 3: Bifunctor
-- ----------------

{-
Bifunctor - это тип с двумя параметрами, к которым можно применять функции независимо.

class Bifunctor p where
  bimap :: (a -> c) -> (b -> d) -> p a b -> p c d
  first :: (a -> c) -> p a b -> p c b
  second :: (b -> d) -> p a b -> p a d
-}

-- Пример 1: Pair - пара значений
data Pair a b = Pair a b deriving (Show, Eq)

-- Functor instance для Pair
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Bifunctor Pair where
  bimap f g (Pair a b) = Pair (f a) (g b)
  first f (Pair a b) = Pair (f a) b
  second g (Pair a b) = Pair a (g b)

-- Пример 2: These - значение типа a, типа b, или обоих типов
data These a b = This a | That b | These a b deriving (Show, Eq)

-- Functor instance для These
instance Functor (These a) where
  fmap _ (This a) = This a
  fmap f (That b) = That (f b)
  fmap f (These a b) = These a (f b)

instance Bifunctor These where
  bimap f _ (This a) = This (f a)
  bimap _ g (That b) = That (g b)
  bimap f g (These a b) = These (f a) (g b)
  
  first f (This a) = This (f a)
  first f (These a b) = These (f a) b
  first _ (That b) = That b
  
  second g (That b) = That (g b)
  second g (These a b) = These a (g b)
  second _ (This a) = This a

-- Часть 4: Profunctor
-- -----------------

{-
Profunctor - это тип с двумя параметрами, который контравариантен по первому
параметру и ковариантен по второму.
-}

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id

-- Пример 1: Function - функция a -> b
instance Profunctor (->) where
  dimap f g h = g . h . f
  lmap f g = g . f
  rmap g h = g . h

-- Пример 2: Kleisli - монадическая функция a -> m b
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Monad m => Profunctor (Kleisli m) where
  dimap f g (Kleisli h) = Kleisli (fmap g . h . f)
  lmap f (Kleisli h) = Kleisli (h . f)
  rmap g (Kleisli h) = Kleisli (fmap g . h)

-- Часть 5: Comonad
-- --------------

{-
Comonad - это двойственность к монаде. Если монада позволяет последовательно выполнять
вычисления с эффектами, то комонада позволяет распаковывать контекст и выполнять
вычисления над ним.
-}

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

instance Comonad Identity where
  extract (Identity x) = x
  duplicate x = Identity x

instance Comonad ((,) e) where
  extract (_, a) = a
  duplicate p@(e, _) = (e, p)

-- Часть 6: Другие продвинутые классы типов
-- --------------------------------------

-- Пример 1: Divisible - контравариантный аналог Applicative
class Contravariant f => Divisible f where
  divide :: (a -> (b, c)) -> f b -> f c -> f a
  conquer :: f a

instance Divisible Predicate where
  divide f (Predicate p) (Predicate q) = Predicate $ \a ->
    let (b, c) = f a
    in p b && q c
  conquer = Predicate $ const True

-- Пример 2: Decidable - контравариантный аналог Alternative
class Divisible f => Decidable f where
  choose :: (a -> Either b c) -> f b -> f c -> f a
  lose :: (a -> Void) -> f a

instance Decidable Predicate where
  choose f (Predicate p) (Predicate q) = Predicate $ \a ->
    case f a of
      Left b -> p b
      Right c -> q c
  lose f = Predicate $ \a -> absurd (f a)

-- Пример 3: Representable - функторы, которые можно представить как функции
class Functor f => Representable f where
  type Rep f :: *
  tabulate :: (Rep f -> a) -> f a
  index :: f a -> Rep f -> a

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Contravariant Functor"
  
  putStrLn $ "isEven 4: " ++ show (runPredicate isEven 4)
  putStrLn $ "isEven 5: " ++ show (runPredicate isEven 5)
  
  putStrLn $ "isEvenLength \"hello\": " ++ show (runPredicate isEvenLength "hello")
  putStrLn $ "isEvenLength \"hi\": " ++ show (runPredicate isEvenLength "hi")
  
  let compareByLengthThenFirstChar = thenCompare byLength byFirstChar
  putStrLn $ "compare \"abc\" \"def\" by length: " ++ show (runComparison byLength "abc" "def")
  putStrLn $ "compare \"abc\" \"def\" by first char: " ++ show (runComparison byFirstChar "abc" "def")
  putStrLn $ "compare \"abc\" \"def\" by length then first char: " ++ show (runComparison compareByLengthThenFirstChar "abc" "def")
  putStrLn $ "compare \"abc\" \"ab\" by length then first char: " ++ show (runComparison compareByLengthThenFirstChar "abc" "ab")

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Bifunctor"
  
  let pair = Pair 5 "hello"
  putStrLn $ "pair: " ++ show pair
  putStrLn $ "bimap (*2) reverse pair: " ++ show (bimap (*2) reverse pair)
  putStrLn $ "first (*2) pair: " ++ show (first (*2) pair)
  putStrLn $ "second reverse pair: " ++ show (second reverse pair)
  
  let these1 = This 5 :: These Int String
  let these2 = That "hello" :: These Int String
  let these3 = These 5 "hello" :: These Int String
  
  putStrLn $ "these1: " ++ show these1
  putStrLn $ "these2: " ++ show these2
  putStrLn $ "these3: " ++ show these3
  
  putStrLn $ "bimap (*2) reverse these1: " ++ show (bimap (*2) reverse these1)
  putStrLn $ "bimap (*2) reverse these2: " ++ show (bimap (*2) reverse these2)
  putStrLn $ "bimap (*2) reverse these3: " ++ show (bimap (*2) reverse these3)

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Profunctor"
  
  let f :: Int -> Int
      f = dimap (+1) (*2) (\x -> x * x)
  putStrLn $ "dimap (+1) (*2) (\\x -> x * x) applied to 5: " ++ show (f 5)
  
  let kleisliFunc = Kleisli $ \x -> if x > 0 then Just (x * 2) else Nothing
  let dimappedKleisli = dimap (+1) show kleisliFunc
  
  putStrLn $ "kleisliFunc applied to 5: " ++ show (runKleisli kleisliFunc 5)
  putStrLn $ "kleisliFunc applied to -1: " ++ show (runKleisli kleisliFunc (-1))
  putStrLn $ "dimappedKleisli applied to 5: " ++ show (runKleisli dimappedKleisli 5)
  putStrLn $ "dimappedKleisli applied to -2: " ++ show (runKleisli dimappedKleisli (-2))

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Comonad"
  
  let identity = Identity 5
  putStrLn $ "extract (Identity 5): " ++ show (extract identity)
  
  let pair = ("hello", 42)
  putStrLn $ "extract (\"hello\", 42): " ++ show (extract pair)
  
  let extendedPair = extend (\(_, n) -> n * 2) pair
  putStrLn $ "extend (\\(_, n) -> n * 2) (\"hello\", 42): " ++ show extendedPair

example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Другие продвинутые классы типов"
  
  let evenAndPositive = divide (\x -> (x, x)) isEven (Predicate (> 0))
  
  putStrLn $ "evenAndPositive 4: " ++ show (runPredicate evenAndPositive 4)
  putStrLn $ "evenAndPositive 5: " ++ show (runPredicate evenAndPositive 5)
  putStrLn $ "evenAndPositive (-2): " ++ show (runPredicate evenAndPositive (-2))
  
  let evenOrPositive = choose (\x -> if even x then Left x else Right x) isEven (Predicate (> 0))
  
  putStrLn $ "evenOrPositive 4: " ++ show (runPredicate evenOrPositive 4)
  putStrLn $ "evenOrPositive 5: " ++ show (runPredicate evenOrPositive 5)
  putStrLn $ "evenOrPositive (-2): " ++ show (runPredicate evenOrPositive (-2))

-- Главная функция
main :: IO ()
main = do
  putStrLn "Продвинутые классы типов в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  
  putStrLn "\nКлючевые моменты о продвинутых классах типов:"
  putStrLn "1. Contravariant - функтор, который позволяет применять функции в обратном направлении"
  putStrLn "2. Bifunctor - тип с двумя параметрами, к которым можно применять функции независимо"
  putStrLn "3. Profunctor - тип с двумя параметрами, контравариантный по первому и ковариантный по второму"
  putStrLn "4. Comonad - двойственность к монаде, позволяет распаковывать контекст"
  putStrLn "5. Divisible и Decidable - контравариантные аналоги Applicative и Alternative"
  putStrLn "6. Representable - функторы, которые можно представить как функции"
