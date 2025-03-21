{-
  Зависимые типы и Liquid Haskell
  
  В этом файле мы рассмотрим приближение к зависимым типам в Haskell
  и использование Liquid Haskell для доказательства свойств программ.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main where

import GHC.TypeLits hiding (Nat, SNat)
import Data.Proxy
import Data.Kind (Type)

-- Часть 1: Введение в зависимые типы
-- ---------------------------------

{-
Зависимые типы - это типы, которые зависят от значений.
В полноценных языках с зависимыми типами (например, Idris, Agda, Coq)
можно писать типы, которые содержат произвольные выражения.

Например, в Idris можно определить тип вектора фиксированной длины:

data Vect : Nat -> Type -> Type where
  Nil  : Vect 0 a
  Cons : a -> Vect n a -> Vect (S n) a

Здесь тип Vect n a зависит от значения n типа Nat.

Haskell не является языком с полноценными зависимыми типами,
но с помощью расширений языка можно эмулировать многие возможности
зависимых типов.
-}

-- Часть 2: Эмуляция зависимых типов в Haskell
-- -----------------------------------------

-- Определение натуральных чисел на уровне типов
data Nat = Zero | Succ Nat

-- Синглтоны для связи значений и типов
data SNat (n :: Nat) where
  SZero :: SNat 'Zero
  SSucc :: SNat n -> SNat ('Succ n)

-- Преобразование SNat в Int
toInt :: SNat n -> Int
toInt SZero = 0
toInt (SSucc n) = 1 + toInt n

-- Определение вектора с длиной на уровне типов
data Vector (n :: Nat) a where
  VNil :: Vector 'Zero a
  VCons :: a -> Vector n a -> Vector ('Succ n) a

-- Безопасная функция head для вектора
vhead :: Vector ('Succ n) a -> a
vhead (VCons x _) = x

-- Безопасная функция tail для вектора
vtail :: Vector ('Succ n) a -> Vector n a
vtail (VCons _ xs) = xs

-- Конкатенация векторов
type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)

vappend :: Vector n a -> Vector m a -> Vector (Add n m) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

-- Безопасный доступ к элементу по индексу
data Fin (n :: Nat) where
  FZero :: Fin ('Succ n)
  FSucc :: Fin n -> Fin ('Succ n)

vindex :: Vector n a -> Fin n -> a
vindex (VCons x _) FZero = x
vindex (VCons _ xs) (FSucc i) = vindex xs i

-- Преобразование вектора в список
vtoList :: Vector n a -> [a]
vtoList VNil = []
vtoList (VCons x xs) = x : vtoList xs

-- Часть 3: Продвинутые примеры эмуляции зависимых типов
-- --------------------------------------------------

-- Пример 1: Типобезопасная реализация replicate
vreplicate :: SNat n -> a -> Vector n a
vreplicate SZero _ = VNil
vreplicate (SSucc n) x = VCons x (vreplicate n x)

-- Пример 2: Типобезопасная реализация zip
vzip :: Vector n a -> Vector n b -> Vector n (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) (vzip xs ys)

-- Пример 3: Типобезопасная реализация map
vmap :: (a -> b) -> Vector n a -> Vector n b
vmap _ VNil = VNil
vmap f (VCons x xs) = VCons (f x) (vmap f xs)

-- Пример 4: Типобезопасная реализация reverse (упрощенная версия)
-- Для демонстрации используем более простую реализацию
vreverse :: Vector n a -> [a]
vreverse = reverse . vtoList

-- Преобразование списка в вектор (для примера)
vfromListSimple :: [a] -> Vector 'Zero a
vfromListSimple _ = VNil

-- Пример 5: Типобезопасная реализация take и drop
type family Min (n :: Nat) (m :: Nat) :: Nat where
  Min 'Zero _ = 'Zero
  Min _ 'Zero = 'Zero
  Min ('Succ n) ('Succ m) = 'Succ (Min n m)

vtake :: SNat m -> Vector n a -> Vector (Min m n) a
vtake SZero _ = VNil
vtake _ VNil = VNil
vtake (SSucc m) (VCons x xs) = VCons x (vtake m xs)

type family Sub (n :: Nat) (m :: Nat) :: Nat where
  Sub n 'Zero = n
  Sub ('Succ n) ('Succ m) = Sub n m
  Sub 'Zero _ = 'Zero  -- Защита от отрицательных чисел

vdrop :: SNat m -> Vector n a -> Vector (Sub n m) a
vdrop SZero xs = xs
vdrop _ VNil = VNil
vdrop (SSucc m) (VCons _ xs) = vdrop m xs

-- Часть 4: Введение в Liquid Haskell
-- --------------------------------

{-
Liquid Haskell - это верификатор для Haskell, который позволяет
добавлять уточненные типы (refinement types) к обычным типам Haskell.

Уточненные типы - это типы с предикатами, которые ограничивают
множество значений, которые может принимать переменная.

Например, в Liquid Haskell можно определить тип натуральных чисел:

{-@ type Nat = {v:Int | v >= 0} @-}

Это означает, что Nat - это подмножество Int, такое что v >= 0.

Liquid Haskell использует SMT-решатель для проверки свойств программ
во время компиляции.

Для использования Liquid Haskell нужно установить его и SMT-решатель:
$ cabal install liquid-types
$ stack install liquid-types
или
$ brew install z3

Затем можно запустить проверку:
$ liquid MyFile.hs

В этом файле мы покажем примеры использования Liquid Haskell,
но для их проверки нужно установить Liquid Haskell и запустить
проверку отдельно.
-}

-- Примеры использования Liquid Haskell

-- Пример 1: Уточненные типы для базовых типов

{-
{-@ type Nat = {v:Int | v >= 0} @-}
{-@ type Pos = {v:Int | v > 0} @-}
{-@ type NonNeg = {v:Int | v >= 0} @-}
{-@ type NonEmpty a = {v:[a] | len v > 0} @-}

{-@ abs :: Int -> Nat @-}
abs :: Int -> Int
abs n | n >= 0 = n
      | otherwise = -n

{-@ factorial :: Nat -> Pos @-}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-@ safeDiv :: Int -> Pos -> Int @-}
safeDiv :: Int -> Int -> Int
safeDiv n d = n `div` d

{-@ head :: NonEmpty a -> a @-}
head :: [a] -> a
head (x:_) = x
head [] = error "empty list"  -- Liquid Haskell обнаружит, что это недостижимо
-}

-- Пример 2: Доказательство свойств функций

{-
{-@ measure len @-}
{-@ len :: [a] -> Nat @-}
len :: [a] -> Int
len [] = 0
len (_:xs) = 1 + len xs

{-@ append :: xs:[a] -> ys:[a] -> {v:[a] | len v == len xs + len ys} @-}
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

{-@ reverse :: xs:[a] -> {v:[a] | len v == len xs} @-}
reverse :: [a] -> [a]
reverse = go []
  where
    {-@ go :: acc:[a] -> xs:[a] -> {v:[a] | len v == len acc + len xs} @-}
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs
-}

-- Пример 3: Доказательство свойств сортировки

{-
{-@ measure sorted @-}
sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:zs) = x <= y && sorted (y:zs)

{-@ measure elems @-}
elems :: Eq a => [a] -> [a]
elems [] = []
elems (x:xs) = x : elems xs

{-@ insertSort :: Ord a => xs:[a] -> {v:[a] | sorted v && elems v == elems xs} @-}
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)

{-@ insert :: Ord a => x:a -> xs:{v:[a] | sorted v} -> {v:[a] | sorted v && elems v == x : elems xs} @-}
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys
-}

-- Часть 5: Продвинутые примеры использования Liquid Haskell
-- ------------------------------------------------------

-- Пример 1: Доказательство свойств векторов

{-
{-@ measure vlen @-}
{-@ vlen :: Vector n a -> {v:Int | v == n} @-}
vlen :: Vector n a -> Int
vlen VNil = 0
vlen (VCons _ xs) = 1 + vlen xs

{-@ vappend :: xs:Vector n a -> ys:Vector m a -> {v:Vector {n + m} a | vlen v == vlen xs + vlen ys} @-}
-}

-- Пример 2: Доказательство свойств бинарных деревьев

{-
data Tree a = Leaf | Node (Tree a) a (Tree a)

{-@ measure height @-}
height :: Tree a -> Int
height Leaf = 0
height (Node l _ r) = 1 + max (height l) (height r)

{-@ measure size @-}
size :: Tree a -> Int
size Leaf = 0
size (Node l _ r) = 1 + size l + size r

{-@ type BalancedTree a = {v:Tree a | isBalanced v} @-}
{-@ measure isBalanced @-}
isBalanced :: Tree a -> Bool
isBalanced Leaf = True
isBalanced (Node l _ r) = isBalanced l && isBalanced r && abs (height l - height r) <= 1

{-@ balanceTree :: Tree a -> BalancedTree a @-}
balanceTree :: Tree a -> Tree a
balanceTree Leaf = Leaf
balanceTree (Node l x r)
  | abs (height l' - height r') <= 1 = Node l' x r'
  | height l' > height r' = rotateRight (Node l' x r')
  | otherwise = rotateLeft (Node l' x r')
  where
    l' = balanceTree l
    r' = balanceTree r

{-@ rotateRight :: {t:Tree a | height (left t) > height (right t) + 1} -> BalancedTree a @-}
rotateRight :: Tree a -> Tree a
rotateRight (Node (Node ll x lr) y r) = Node ll x (Node lr y r)
rotateRight t = t  -- Недостижимо, но нужно для полноты

{-@ rotateLeft :: {t:Tree a | height (right t) > height (left t) + 1} -> BalancedTree a @-}
rotateLeft :: Tree a -> Tree a
rotateLeft (Node l x (Node rl y rr)) = Node (Node l x rl) y rr
rotateLeft t = t  -- Недостижимо, но нужно для полноты
-}

-- Пример 3: Доказательство свойств монад

{-
{-@ measure isJust @-}
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

{-@ measure fromJust @-}
fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

{-@ bindMaybe :: m:Maybe a -> f:(a -> Maybe b) -> {v:Maybe b | isJust v <=> isJust m && isJust (f (fromJust m))} @-}
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _ = Nothing
bindMaybe (Just x) f = f x

{-@ returnMaybe :: x:a -> {v:Maybe a | isJust v && fromJust v == x} @-}
returnMaybe :: a -> Maybe a
returnMaybe = Just
-}

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Векторы с зависимыми типами"
  
  let v1 = VCons 1 (VCons 2 (VCons 3 VNil))
  let v2 = VCons 4 (VCons 5 VNil)
  let v3 = vappend v1 v2
  
  putStrLn $ "v1 = " ++ show (vtoList v1)
  putStrLn $ "v2 = " ++ show (vtoList v2)
  putStrLn $ "v1 ++ v2 = " ++ show (vtoList v3)
  
  putStrLn $ "Первый элемент v1: " ++ show (vhead v1)
  putStrLn $ "Второй элемент v1: " ++ show (vhead (vtail v1))

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Продвинутые операции с векторами"
  
  let v1 = VCons 1 (VCons 2 (VCons 3 VNil))
  let v2 = vreplicate (SSucc (SSucc (SSucc SZero))) 42
  
  putStrLn $ "v1 = " ++ show (vtoList v1)
  putStrLn $ "replicate 3 42 = " ++ show (vtoList v2)
  
  let v3 = vmap (*2) v1
  putStrLn $ "map (*2) v1 = " ++ show (vtoList v3)
  
  let v4 = vreverse v1
  putStrLn $ "reverse v1 = " ++ show v4
  
  let v5 = vzip v1 v3
  putStrLn $ "zip v1 (map (*2) v1) = " ++ show (vtoList v5)

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Liquid Haskell (примеры без проверки)"
  
  putStrLn "Liquid Haskell позволяет добавлять уточненные типы к обычным типам Haskell."
  putStrLn "Например, можно определить тип натуральных чисел:"
  putStrLn "{-@ type Nat = {v:Int | v >= 0} @-}"
  
  putStrLn "\nМожно доказывать свойства функций:"
  putStrLn "{-@ append :: xs:[a] -> ys:[a] -> {v:[a] | len v == len xs + len ys} @-}"
  
  putStrLn "\nМожно доказывать свойства сортировки:"
  putStrLn "{-@ insertSort :: Ord a => xs:[a] -> {v:[a] | sorted v && elems v == elems xs} @-}"
  
  putStrLn "\nМожно доказывать свойства сложных структур данных:"
  putStrLn "{-@ balanceTree :: Tree a -> BalancedTree a @-}"
  
  putStrLn "\nДля использования Liquid Haskell нужно установить его и SMT-решатель,"
  putStrLn "а затем запустить проверку: $ liquid MyFile.hs"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Зависимые типы и Liquid Haskell\n"
  
  example1
  example2
  example3
  
  putStrLn "\nКлючевые моменты о зависимых типах и Liquid Haskell:"
  putStrLn "1. Зависимые типы - это типы, которые зависят от значений"
  putStrLn "2. Haskell не является языком с полноценными зависимыми типами, но можно эмулировать многие возможности"
  putStrLn "3. Синглтоны связывают значения и типы, позволяя переносить информацию с уровня типов на уровень значений"
  putStrLn "4. Liquid Haskell - это верификатор для Haskell, который позволяет добавлять уточненные типы"
  putStrLn "5. Уточненные типы - это типы с предикатами, которые ограничивают множество значений"
  putStrLn "6. Liquid Haskell использует SMT-решатель для проверки свойств программ во время компиляции"
