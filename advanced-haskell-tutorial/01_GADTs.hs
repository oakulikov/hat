{-
  Обобщенные алгебраические типы данных (GADTs) в Haskell
  
  В этом файле мы рассмотрим обобщенные алгебраические типы данных (GADTs),
  их синтаксис, применение и преимущества перед обычными ADT.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Maybe (isJust)

-- Часть 1: Введение в GADTs
-- -------------------------

-- Обычный алгебраический тип данных (ADT) для выражений
data Expr1
  = Lit1 Int
  | Add1 Expr1 Expr1
  | IsZero1 Expr1
  deriving (Show)

-- Проблема с обычными ADT: нет гарантии типобезопасности
-- Например, мы можем создать некорректное выражение:
-- IsZero1 (Add1 (Lit1 1) (Lit1 2))
-- Это выражение проверяет, равно ли 1 + 2 нулю, что имеет смысл,
-- но результатом IsZero1 должно быть булево значение, а не число.

-- Вычисление выражения с обычным ADT требует проверок во время выполнения
eval1 :: Expr1 -> Maybe (Either Int Bool)
eval1 (Lit1 n) = Just (Left n)
eval1 (Add1 e1 e2) = do
  v1 <- eval1 e1
  v2 <- eval1 e2
  case (v1, v2) of
    (Left n1, Left n2) -> Just (Left (n1 + n2))
    _ -> Nothing  -- Ошибка типа во время выполнения
eval1 (IsZero1 e) = do
  v <- eval1 e
  case v of
    Left n -> Just (Right (n == 0))
    _ -> Nothing  -- Ошибка типа во время выполнения

-- GADT для типобезопасных выражений
data Expr :: * -> * where
  Lit    :: Int -> Expr Int
  Add    :: Expr Int -> Expr Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool
  If     :: Expr Bool -> Expr a -> Expr a -> Expr a

-- Определение Show для Expr
instance Show (Expr Int) where
  show (Lit n) = "Lit " ++ show n
  show (Add e1 e2) = "Add (" ++ show e1 ++ ") (" ++ show e2 ++ ")"
  show (If cond t e) = "If (" ++ show cond ++ ") (" ++ show t ++ ") (" ++ show e ++ ")"

instance Show (Expr Bool) where
  show (IsZero e) = "IsZero (" ++ show e ++ ")"
  show (If cond t e) = "If (" ++ show cond ++ ") (" ++ show t ++ ") (" ++ show e ++ ")"

-- Теперь компилятор гарантирует, что мы не можем создать некорректное выражение
-- Например, следующее выражение не скомпилируется:
-- IsZero (If (IsZero (Lit 0)) (Lit 1) (Lit 2))
-- Потому что IsZero ожидает аргумент типа Expr Int, а If возвращает Expr Bool

-- Вычисление выражения с GADT не требует проверок во время выполнения
eval :: Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (IsZero e) = eval e == 0
eval (If cond thenExpr elseExpr) =
  if eval cond
    then eval thenExpr
    else eval elseExpr

-- Примеры выражений с GADT
expr1 :: Expr Int
expr1 = Add (Lit 1) (Lit 2)

expr2 :: Expr Bool
expr2 = IsZero (Lit 0)

expr3 :: Expr Int
expr3 = If (IsZero (Lit 0)) (Lit 42) (Lit 0)

-- Часть 2: Продвинутые примеры использования GADTs
-- -----------------------------------------------

-- Пример 1: Типобезопасный интерпретатор простого языка
data Term :: * -> * where
  TLit   :: Int -> Term Int
  TBool  :: Bool -> Term Bool
  TAdd   :: Term Int -> Term Int -> Term Int
  TMul   :: Term Int -> Term Int -> Term Int
  TEq    :: Term Int -> Term Int -> Term Bool
  TIf    :: Term Bool -> Term a -> Term a -> Term a
  TLambda :: (Term Int -> Term b) -> Term (Int -> b)
  TApp   :: Term (Int -> b) -> Term Int -> Term b

-- Определение Show для Term
instance Show (Term Int) where
  show (TLit n) = "TLit " ++ show n
  show (TAdd t1 t2) = "TAdd (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (TMul t1 t2) = "TMul (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (TIf cond t e) = "TIf (" ++ show cond ++ ") (" ++ show t ++ ") (" ++ show e ++ ")"

instance Show (Term Bool) where
  show (TBool b) = "TBool " ++ show b
  show (TEq t1 t2) = "TEq (" ++ show t1 ++ ") (" ++ show t2 ++ ")"
  show (TIf cond t e) = "TIf (" ++ show cond ++ ") (" ++ show t ++ ") (" ++ show e ++ ")"

-- Общий случай для Term (a -> b)
instance Show (Term (Int -> b)) where
  show (TLambda _) = "TLambda <function>"
  show (TApp _ _) = "TApp <function> <value>"

-- Интерпретатор для Term
interpretTerm :: Term a -> a
interpretTerm (TLit n) = n
interpretTerm (TBool b) = b
interpretTerm (TAdd t1 t2) = interpretTerm t1 + interpretTerm t2
interpretTerm (TMul t1 t2) = interpretTerm t1 * interpretTerm t2
interpretTerm (TEq t1 t2) = interpretTerm t1 == interpretTerm t2
interpretTerm (TIf cond t f) =
  if interpretTerm cond
    then interpretTerm t
    else interpretTerm f
interpretTerm (TLambda f) = \x -> interpretTerm (f (TLit x))
interpretTerm (TApp f x) = (interpretTerm f) (interpretTerm x)

-- Пример 2: Типобезопасное представление гетерогенных списков
data HList :: [*] -> * where
  HNil  :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

-- Функции для работы с гетерогенными списками
hHead :: HList (a ': as) -> a
hHead (HCons x _) = x

hTail :: HList (a ': as) -> HList as
hTail (HCons _ xs) = xs

-- Пример 3: Типобезопасное представление векторов фиксированной длины
data Vector :: * -> Nat -> * where
  VNil  :: Vector a 'Zero
  VCons :: a -> Vector a n -> Vector a ('Succ n)

-- Определение натуральных чисел на уровне типов
data Nat = Zero | Succ Nat

-- Тип-уровневая функция для сложения натуральных чисел
type family Add (n :: Nat) (m :: Nat) :: Nat where
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)

-- Конкатенация векторов с сохранением информации о длине
vappend :: Vector a n -> Vector a m -> Vector a (Add n m)
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

-- Пример 4: Типобезопасное представление абстрактного синтаксического дерева
data AST :: * -> * where
  ASTInt    :: Int -> AST Int
  ASTBool   :: Bool -> AST Bool
  ASTString :: String -> AST String
  ASTAdd    :: AST Int -> AST Int -> AST Int
  ASTConcat :: AST String -> AST String -> AST String
  ASTNot    :: AST Bool -> AST Bool
  ASTPrint  :: Show a => AST a -> AST ()

-- Определение Show для AST
instance Show (AST Int) where
  show (ASTInt n) = "ASTInt " ++ show n
  show (ASTAdd a b) = "ASTAdd (" ++ show a ++ ") (" ++ show b ++ ")"

instance Show (AST Bool) where
  show (ASTBool b) = "ASTBool " ++ show b
  show (ASTNot a) = "ASTNot (" ++ show a ++ ")"

instance Show (AST String) where
  show (ASTString s) = "ASTString " ++ show s
  show (ASTConcat a b) = "ASTConcat (" ++ show a ++ ") (" ++ show b ++ ")"

instance Show (AST ()) where
  show (ASTPrint _) = "ASTPrint <value>"

-- Интерпретатор для AST
interpretAST :: AST a -> IO a
interpretAST (ASTInt n) = return n
interpretAST (ASTBool b) = return b
interpretAST (ASTString s) = return s
interpretAST (ASTAdd a b) = do
  a' <- interpretAST a
  b' <- interpretAST b
  return (a' + b')
interpretAST (ASTConcat a b) = do
  a' <- interpretAST a
  b' <- interpretAST b
  return (a' ++ b')
interpretAST (ASTNot a) = do
  a' <- interpretAST a
  return (not a')
interpretAST (ASTPrint a) = do
  a' <- interpretAST a
  print a'
  return ()

-- Часть 3: Практическое применение GADTs
-- -------------------------------------

-- Пример 1: Типобезопасный DSL для запросов к базе данных
data Table = Users | Products | Orders

data Field :: Table -> * -> * where
  UserId       :: Field Users Int
  UserName     :: Field Users String
  UserEmail    :: Field Users String
  ProductId    :: Field Products Int
  ProductName  :: Field Products String
  ProductPrice :: Field Products Double
  OrderId      :: Field Orders Int
  OrderUserId  :: Field Orders Int
  OrderDate    :: Field Orders String

data Query :: Table -> * -> * where
  Select :: Field t a -> Query t a
  Where  :: Query t a -> (a -> Bool) -> Query t a
  Join   :: Query t1 a -> Query t2 b -> (a -> b -> Bool) -> Query t1 (a, b)

-- Пример 2: Типобезопасный DSL для построения HTML
data HTML :: * -> * where
  Text   :: String -> HTML ()
  Div    :: HTML a -> HTML ()
  P      :: HTML a -> HTML ()
  H1     :: HTML a -> HTML ()
  Input  :: HTML String
  Button :: String -> HTML ()
  Form   :: HTML a -> (a -> HTML b) -> HTML b

-- Пример 3: Типобезопасный DSL для построения графов
data Graph :: * -> * where
  Node   :: a -> Graph a
  Edge   :: Graph a -> Graph a -> Graph (a, a)
  Subgraph :: Graph a -> Graph a
  Empty  :: Graph ()

-- Часть 4: Преимущества и недостатки GADTs
-- ---------------------------------------

{-
Преимущества GADTs:

1. Типобезопасность: GADTs позволяют выразить более сложные инварианты на уровне типов,
   что помогает обнаруживать ошибки во время компиляции, а не во время выполнения.

2. Выразительность: GADTs позволяют создавать более выразительные и точные модели предметной области.

3. Устранение тегов: В некоторых случаях компилятор может оптимизировать код, устраняя теги,
   которые обычно используются для представления сумм типов.

4. Паттерн-матчинг с уточнением типов: При сопоставлении с образцом для GADT
   компилятор может уточнить типы на основе конструкторов.

Недостатки GADTs:

1. Сложность: GADTs могут быть сложнее для понимания и использования,
   особенно для программистов, не знакомых с продвинутыми системами типов.

2. Ограничения вывода типов: В некоторых случаях компилятор может не иметь
   достаточно информации для вывода типов, что требует явных аннотаций типов.

3. Взаимодействие с другими расширениями: GADTs могут взаимодействовать
   с другими расширениями языка в неочевидных способах.
-}

-- Часть 5: Примеры использования GADTs в реальных проектах
-- ------------------------------------------------------

{-
1. Библиотека servant использует GADTs для типобезопасного представления API.

2. Библиотека lens использует GADTs для представления различных видов оптик.

3. Библиотека accelerate использует GADTs для типобезопасного представления
   массивов и операций над ними для параллельных вычислений.

4. Библиотека free использует GADTs для представления свободных монад.

5. Библиотека recursion-schemes использует GADTs для представления
   различных схем рекурсии.
-}

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Типобезопасные выражения с GADT"
  
  putStrLn $ "Выражение 1: " ++ show expr1
  putStrLn $ "Результат: " ++ show (eval expr1)
  
  putStrLn $ "Выражение 2: " ++ show expr2
  putStrLn $ "Результат: " ++ show (eval expr2)
  
  putStrLn $ "Выражение 3: " ++ show expr3
  putStrLn $ "Результат: " ++ show (eval expr3)

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Типобезопасный интерпретатор"
  
  let term1 = TAdd (TLit 1) (TLit 2)
  let term2 = TEq (TLit 3) (TAdd (TLit 1) (TLit 2))
  let term3 = TIf (TEq (TLit 3) (TAdd (TLit 1) (TLit 2)))
                  (TLit 42)
                  (TLit 0)
  
  putStrLn $ "Терм 1: " ++ show term1
  putStrLn $ "Результат: " ++ show (interpretTerm term1)
  
  putStrLn $ "Терм 2: " ++ show term2
  putStrLn $ "Результат: " ++ show (interpretTerm term2)
  
  putStrLn $ "Терм 3: " ++ show term3
  putStrLn $ "Результат: " ++ show (interpretTerm term3)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Обобщенные алгебраические типы данных (GADTs) в Haskell\n"
  
  example1
  example2
  
  putStrLn "\nКлючевые моменты о GADTs:"
  putStrLn "1. GADTs позволяют указывать типы результатов для каждого конструктора данных"
  putStrLn "2. GADTs обеспечивают типобезопасность на уровне компиляции"
  putStrLn "3. GADTs позволяют создавать более выразительные и точные модели предметной области"
  putStrLn "4. GADTs часто используются для создания типобезопасных DSL"
  putStrLn "5. GADTs могут взаимодействовать с другими расширениями языка, такими как DataKinds и TypeFamilies"
