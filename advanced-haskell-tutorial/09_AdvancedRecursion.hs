{-
  Продвинутая рекурсия в Haskell
  
  В этом файле мы рассмотрим продвинутые техники рекурсии,
  включая схемы рекурсии, катаморфизмы, анаморфизмы и другие
  рекурсивные схемы.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Arrow ((>>>))
import Data.List (intercalate)
import Data.Monoid (Sum(..))
import Data.Char (toUpper)

-- Определение базовых типов для схем рекурсии
class Recursive t where
  project :: t -> Base t t
  cata :: Functor (Base t) => (Base t a -> a) -> t -> a
  cata f = f . fmap (cata f) . project

class Corecursive t where
  embed :: Base t t -> t
  ana :: Functor (Base t) => (a -> Base t a) -> a -> t
  ana g = embed . fmap (ana g) . g

type family Base t :: * -> *

-- Часть 1: Введение в продвинутую рекурсию
-- ------------------------------------

{-
Рекурсия - это мощный инструмент в функциональном программировании, но обычная рекурсия
может быть сложной для понимания и поддержки. Продвинутые техники рекурсии позволяют
абстрагировать рекурсивные паттерны и сделать код более модульным и понятным.

Основные концепции продвинутой рекурсии:

1. Схемы рекурсии (recursion schemes) - абстракции рекурсивных паттернов
2. Катаморфизмы (catamorphisms) - обобщенная свертка (fold)
3. Анаморфизмы (anamorphisms) - обобщенная развертка (unfold)
4. Гиломорфизмы (hylomorphisms) - композиция катаморфизма и анаморфизма
5. Парамофизмы (paramorphisms) - свертка с доступом к промежуточным результатам
6. Апоморфизмы (apomorphisms) - развертка с возможностью раннего завершения
7. Зигоморфизмы (zygomorphisms) - свертка с дополнительным накоплением

В этом файле мы рассмотрим эти концепции и их применение в Haskell.
-}

-- Часть 2: Рекурсивные типы данных и функторы
-- ---------------------------------------

-- Рекурсивный тип данных для натуральных чисел
data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Рекурсивный тип данных для списков
data List a = Nil | Cons a (List a) deriving (Show, Eq, Functor)

-- Рекурсивный тип данных для бинарных деревьев
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq, Functor)

-- Рекурсивный тип данных для выражений
data Expr
  = Lit Int
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  deriving (Show, Eq)

-- Функтор для натуральных чисел
data NatF a = ZeroF | SuccF a deriving (Show, Eq, Functor)

-- Функтор для списков
data ListF a b = NilF | ConsF a b deriving (Show, Eq, Functor)

-- Функтор для бинарных деревьев
data TreeF a b = LeafF a | NodeF b b deriving (Show, Eq, Functor)

-- Функтор для выражений
data ExprF a
  = LitF Int
  | AddF a a
  | MulF a a
  | NegF a
  deriving (Show, Eq, Functor)

-- Изоморфизмы между рекурсивными типами и их функторами
type instance Base Nat = NatF
instance Recursive Nat where
  project Zero = ZeroF
  project (Succ n) = SuccF n
instance Corecursive Nat where
  embed ZeroF = Zero
  embed (SuccF n) = Succ n

type instance Base (List a) = ListF a
instance Recursive (List a) where
  project Nil = NilF
  project (Cons x xs) = ConsF x xs
instance Corecursive (List a) where
  embed NilF = Nil
  embed (ConsF x xs) = Cons x xs

type instance Base (Tree a) = TreeF a
instance Recursive (Tree a) where
  project (Leaf x) = LeafF x
  project (Node l r) = NodeF l r
instance Corecursive (Tree a) where
  embed (LeafF x) = Leaf x
  embed (NodeF l r) = Node l r

type instance Base Expr = ExprF
instance Recursive Expr where
  project (Lit n) = LitF n
  project (Add e1 e2) = AddF e1 e2
  project (Mul e1 e2) = MulF e1 e2
  project (Neg e) = NegF e
instance Corecursive Expr where
  embed (LitF n) = Lit n
  embed (AddF e1 e2) = Add e1 e2
  embed (MulF e1 e2) = Mul e1 e2
  embed (NegF e) = Neg e

-- Часть 3: Катаморфизмы (свертки)
-- ----------------------------

-- Преобразование Nat в Int
natToInt :: Nat -> Int
natToInt = cata $ \case
  ZeroF -> 0
  SuccF n -> n + 1

-- Сумма элементов списка
sumList :: Num a => List a -> a
sumList = cata $ \case
  NilF -> 0
  ConsF x sum -> x + sum

-- Подсчет листьев в дереве
countLeaves :: Tree a -> Int
countLeaves = cata $ \case
  LeafF _ -> 1
  NodeF left right -> left + right

-- Вычисление выражения
evalExpr :: Expr -> Int
evalExpr = cata $ \case
  LitF n -> n
  AddF a b -> a + b
  MulF a b -> a * b
  NegF a -> -a

-- Часть 4: Анаморфизмы (развертки)
-- -----------------------------

-- Создание натурального числа из Int
intToNat :: Int -> Nat
intToNat = ana $ \n ->
  if n <= 0 then ZeroF else SuccF (n - 1)

-- Создание списка из [a]
listFromList :: [a] -> List a
listFromList = ana $ \xs ->
  case xs of
    [] -> NilF
    (x:xs') -> ConsF x xs'

-- Создание сбалансированного дерева из списка
balancedTree :: [a] -> Tree a
balancedTree = ana $ \xs ->
  case xs of
    [x] -> LeafF x
    xs' -> let (left, right) = splitAt (length xs' `div` 2) xs'
           in NodeF left right

-- Создание выражения для вычисления факториала
factExpr :: Int -> Expr
factExpr n = if n <= 1 
  then Lit 1
  else Mul (Lit n) (factExpr (n - 1))

-- Часть 5: Гиломорфизмы (композиция катаморфизма и анаморфизма)
-- ---------------------------------------------------------

-- Гиломорфизм - это композиция катаморфизма и анаморфизма
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = f . fmap (hylo f g) . g

-- Примеры использования гиломорфизмов

-- Вычисление факториала
factorial :: Int -> Int
factorial = hylo
  (\case
    ZeroF -> 1
    SuccF n -> (n + 1) * n)
  (\n -> if n <= 0 then ZeroF else SuccF (n - 1))

-- Быстрая сортировка
quicksort :: Ord a => [a] -> [a]
quicksort = hylo
  (\case
    NilF -> []
    ConsF (pivot, (less, greater)) sorted -> less ++ [pivot] ++ greater)
  (\case
    [] -> NilF
    (pivot:xs) -> ConsF (pivot, partition (<pivot) (>=pivot) xs) xs)
  where
    partition p1 p2 xs = (filter p1 xs, filter p2 xs)

-- Вычисление чисел Фибоначчи
data FibF a = FibZero | FibOne | FibNode a a deriving (Functor)

fibonacci :: Int -> Integer
fibonacci = hylo alg coalg
  where
    alg FibZero = 0
    alg FibOne = 1
    alg (FibNode a b) = a + b
    
    coalg n
      | n == 0 = FibZero
      | n == 1 = FibOne
      | otherwise = FibNode (n - 1) (n - 2)

-- Часть 6: Парамофизмы и апоморфизмы
-- -------------------------------

-- Вычисление факториала с использованием парамофизма
factorialPara :: Nat -> Integer
factorialPara Zero = 1
factorialPara (Succ n) = fromIntegral (natToInt (Succ n)) * factorialPara n

-- Создание списка с ранним завершением
takeList :: Int -> [a] -> List a
takeList n xs = take' n xs
  where
    take' :: Int -> [a] -> List a
    take' 0 _ = Nil
    take' _ [] = Nil
    take' n (x:xs) = Cons x (take' (n-1) xs)

-- Часть 7: Зигоморфизмы и другие схемы рекурсии
-- -----------------------------------------

-- Проверка, является ли дерево сбалансированным
isBalanced :: Tree a -> Bool
isBalanced (Leaf _) = True
isBalanced (Node l r) = 
  isBalanced l && isBalanced r && 
  abs (treeHeight l - treeHeight r) <= 1
  where
    treeHeight :: Tree a -> Int
    treeHeight (Leaf _) = 1
    treeHeight (Node l r) = 1 + max (treeHeight l) (treeHeight r)

-- Вычисление среднего арифметического списка
average :: Fractional a => List a -> a
average list = sum / count
  where
    (sum, count) = go list
    go :: Fractional a => List a -> (a, a)
    go Nil = (0, 0)
    go (Cons x xs) = 
      let (sum', count') = go xs
      in (x + sum', 1 + count')

-- Часть 8: Практическое применение схем рекурсии
-- -----------------------------------------

-- Пример 1: Обработка JSON-подобных данных
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

-- Функтор для JsonValue
data JsonValueF a
  = JsonNullF
  | JsonBoolF Bool
  | JsonNumberF Double
  | JsonStringF String
  | JsonArrayF [a]
  | JsonObjectF [(String, a)]
  deriving (Show, Eq, Functor)

-- Изоморфизм между JsonValue и JsonValueF
type instance Base JsonValue = JsonValueF
instance Recursive JsonValue where
  project JsonNull = JsonNullF
  project (JsonBool b) = JsonBoolF b
  project (JsonNumber n) = JsonNumberF n
  project (JsonString s) = JsonStringF s
  project (JsonArray arr) = JsonArrayF arr
  project (JsonObject obj) = JsonObjectF obj
instance Corecursive JsonValue where
  embed JsonNullF = JsonNull
  embed (JsonBoolF b) = JsonBool b
  embed (JsonNumberF n) = JsonNumber n
  embed (JsonStringF s) = JsonString s
  embed (JsonArrayF arr) = JsonArray arr
  embed (JsonObjectF obj) = JsonObject obj

-- Преобразование JsonValue в строку
jsonToString :: JsonValue -> String
jsonToString = cata $ \case
  JsonNullF -> "null"
  JsonBoolF True -> "true"
  JsonBoolF False -> "false"
  JsonNumberF n -> show n
  JsonStringF s -> "\"" ++ s ++ "\""
  JsonArrayF elements -> "[" ++ intercalate ", " elements ++ "]"
  JsonObjectF pairs -> "{" ++ intercalate ", " (map (\(k, v) -> "\"" ++ k ++ "\": " ++ v) pairs) ++ "}"

-- Пример 2: Обработка абстрактного синтаксического дерева
data AST
  = ASTLit Int
  | ASTVar String
  | ASTAdd AST AST
  | ASTMul AST AST
  | ASTLet String AST AST  -- let x = e1 in e2
  deriving (Show, Eq)

-- Функтор для AST
data ASTF a
  = ASTLitF Int
  | ASTVarF String
  | ASTAddF a a
  | ASTMulF a a
  | ASTLetF String a a
  deriving (Show, Eq, Functor)

-- Изоморфизм между AST и ASTF
type instance Base AST = ASTF
instance Recursive AST where
  project (ASTLit n) = ASTLitF n
  project (ASTVar s) = ASTVarF s
  project (ASTAdd e1 e2) = ASTAddF e1 e2
  project (ASTMul e1 e2) = ASTMulF e1 e2
  project (ASTLet x e1 e2) = ASTLetF x e1 e2
instance Corecursive AST where
  embed (ASTLitF n) = ASTLit n
  embed (ASTVarF s) = ASTVar s
  embed (ASTAddF e1 e2) = ASTAdd e1 e2
  embed (ASTMulF e1 e2) = ASTMul e1 e2
  embed (ASTLetF x e1 e2) = ASTLet x e1 e2

-- Вычисление AST с использованием окружения
type Env = [(String, Int)]

-- Вычисление AST с использованием окружения
evalAST :: Env -> AST -> Int
evalAST env ast = runEval env ast
  where
    runEval :: Env -> AST -> Int
    runEval env (ASTLit n) = n
    runEval env (ASTVar s) = case lookup s env of
      Just v -> v
      Nothing -> error $ "Undefined variable: " ++ s
    runEval env (ASTAdd e1 e2) = runEval env e1 + runEval env e2
    runEval env (ASTMul e1 e2) = runEval env e1 * runEval env e2
    runEval env (ASTLet x e1 e2) = 
      let v1 = runEval env e1
          env' = (x, v1) : env
      in runEval env' e2

-- Оптимизация AST
optimizeAST :: AST -> AST
optimizeAST = cata $ \case
  ASTAddF (ASTLit 0) e -> e
  ASTAddF e (ASTLit 0) -> e
  ASTMulF (ASTLit 1) e -> e
  ASTMulF e (ASTLit 1) -> e
  ASTMulF (ASTLit 0) _ -> ASTLit 0
  ASTMulF _ (ASTLit 0) -> ASTLit 0
  ASTLetF x e1 e2 -> if occursIn x e2 then ASTLet x e1 e2 else e2
  f -> embed f
  where
    occursIn :: String -> AST -> Bool
    occursIn x (ASTVar y) = x == y
    occursIn x (ASTLet y e1 e2) = occursIn x e1 || (x /= y && occursIn x e2)
    occursIn x (ASTAdd e1 e2) = occursIn x e1 || occursIn x e2
    occursIn x (ASTMul e1 e2) = occursIn x e1 || occursIn x e2
    occursIn _ _ = False

-- Пример 3: Обработка файловой системы
data FileSystem
  = File String String  -- имя и содержимое
  | Directory String [FileSystem]  -- имя и содержимое
  deriving (Show, Eq)

-- Функтор для FileSystem
data FileSystemF a
  = FileF String String
  | DirectoryF String [a]
  deriving (Show, Eq, Functor)

-- Изоморфизм между FileSystem и FileSystemF
type instance Base FileSystem = FileSystemF
instance Recursive FileSystem where
  project (File name content) = FileF name content
  project (Directory name contents) = DirectoryF name contents
instance Corecursive FileSystem where
  embed (FileF name content) = File name content
  embed (DirectoryF name contents) = Directory name contents

-- Подсчет общего размера файловой системы
totalSize :: FileSystem -> Int
totalSize = cata $ \case
  FileF _ content -> length content
  DirectoryF _ sizes -> sum sizes

-- Поиск файлов по имени
findFiles :: String -> FileSystem -> [String]
findFiles pattern = cata $ \case
  FileF name _ -> if pattern `isInfixOf` name then [name] else []
  DirectoryF name results -> concat results
  where
    isInfixOf :: String -> String -> Bool
    isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
    
    isPrefixOf :: String -> String -> Bool
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

-- Преобразование файловой системы (например, изменение имен файлов)
mapFileSystem :: (String -> String) -> FileSystem -> FileSystem
mapFileSystem f = cata $ \case
  FileF name content -> File (f name) content
  DirectoryF name contents -> Directory (f name) contents

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Катаморфизмы"
  
  let nat5 = Succ (Succ (Succ (Succ (Succ Zero))))
  putStrLn $ "natToInt " ++ show nat5 ++ " = " ++ show (natToInt nat5)
  
  let list123 = Cons 1 (Cons 2 (Cons 3 Nil))
  putStrLn $ "sumList " ++ show list123 ++ " = " ++ show (sumList list123)
  
  let tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
  putStrLn $ "countLeaves " ++ show tree ++ " = " ++ show (countLeaves tree)
  
  let expr = Add (Mul (Lit 2) (Lit 3)) (Neg (Lit 1))
  putStrLn $ "evalExpr " ++ show expr ++ " = " ++ show (evalExpr expr)

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Анаморфизмы"
  
  let nat5 = intToNat 5
  putStrLn $ "intToNat 5 = " ++ show nat5
  
  let list123 = listFromList [1, 2, 3]
  putStrLn $ "listFromList [1, 2, 3] = " ++ show list123
  
  let tree = balancedTree [1, 2, 3, 4, 5]
  putStrLn $ "balancedTree [1, 2, 3, 4, 5] = " ++ show tree
  
  let factExpr5 = factExpr 5
  putStrLn $ "factExpr 5 = " ++ show factExpr5
  putStrLn $ "evalExpr (factExpr 5) = " ++ show (evalExpr factExpr5)

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Гиломорфизмы"
  
  putStrLn $ "factorial 5 = " ++ show (factorial 5)
  putStrLn $ "quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5] = " ++ show (quicksort [3, 1, 4, 1, 5, 9, 2, 6, 5])
  putStrLn $ "fibonacci 10 = " ++ show (fibonacci 10)

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Парамофизмы и апоморфизмы"
  
  let nat5 = intToNat 5
  putStrLn $ "factorialPara " ++ show nat5 ++ " = " ++ show (factorialPara nat5)
  
  let list = listFromList [1..10]
  let list5 = takeList 5 [1..10]
  putStrLn $ "takeList 5 [1..10] = " ++ show list5

example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Практическое применение"
  
  let json = JsonObject
        [ ("name", JsonString "John")
        , ("age", JsonNumber 30)
        , ("isAdmin", JsonBool True)
        , ("address", JsonObject
            [ ("city", JsonString "New York")
            , ("zip", JsonNumber 10001)
            ])
        , ("hobbies", JsonArray
            [ JsonString "reading"
            , JsonString "programming"
            ])
        ]
  
  putStrLn $ "JSON to string: " ++ jsonToString json
  
  let ast = ASTLet "x" (ASTLit 5) (ASTAdd (ASTVar "x") (ASTMul (ASTVar "x") (ASTLit 2)))
  putStrLn $ "AST: " ++ show ast
  putStrLn $ "Eval AST: " ++ show (evalAST [] ast)
  
  let fs = Directory "root"
        [ File "file1.txt" "Hello, world!"
        , Directory "docs"
            [ File "readme.md" "# Documentation"
            , File "guide.txt" "User guide"
            ]
        , File "file2.txt" "Another file"
        ]
  
  putStrLn $ "Total size: " ++ show (totalSize fs)
  putStrLn $ "Find files with 'file': " ++ show (findFiles "file" fs)
  putStrLn $ "Map file system (uppercase): " ++ show (mapFileSystem (map toUpper) fs)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Продвинутая рекурсия в Haskell\n"
  
  example1
  example2
  example3
  example4
  example5
  
  putStrLn "\nКлючевые моменты о продвинутой рекурсии:"
  putStrLn "1. Схемы рекурсии абстрагируют рекурсивные паттерны и делают код более модульным"
  putStrLn "2. Катаморфизмы (cata) - это обобщенные свертки (fold)"
  putStrLn "3. Анаморфизмы (ana) - это обобщенные развертки (unfold)"
  putStrLn "4. Гиломорфизмы (hylo) - это композиция катаморфизма и анаморфизма"
  putStrLn "5. Парамофизмы (para) - это свертки с доступом к промежуточным результатам"
  putStrLn "6. Апоморфизмы (apo) - это развертки с возможностью раннего завершения"
  putStrLn "7. Схемы рекурсии особенно полезны при работе со сложными рекурсивными структурами данных"
