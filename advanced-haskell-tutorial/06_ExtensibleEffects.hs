



{-
  Расширяемые эффекты в Haskell
  
  В этом файле мы рассмотрим расширяемые эффекты - альтернативу
  трансформерам монад для композиции эффектов.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Control.Applicative (liftA2)
import Control.Monad (ap, liftM)
import Data.Functor.Identity

-- Часть 1: Введение в расширяемые эффекты
-- ------------------------------------

{-
Расширяемые эффекты - это подход к моделированию эффектов в функциональном программировании,
который является альтернативой трансформерам монад. Основные преимущества расширяемых эффектов:

1. Модульность: эффекты можно добавлять и удалять независимо друг от друга
2. Типобезопасность: типы гарантируют, что программа использует только доступные эффекты
3. Производительность: расширяемые эффекты могут быть более эффективными, чем трансформеры монад
4. Простота: расширяемые эффекты часто проще в использовании, чем трансформеры монад

В этом файле мы рассмотрим основные концепции расширяемых эффектов и их применение.
-}

-- Часть 2: Основные концепции расширяемых эффектов
-- -------------------------------------------

-- Для простоты мы будем использовать упрощенную версию расширяемых эффектов,
-- основанную на Free монадах и функторах эффектов.

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

-- Часть 3: Определение базовых эффектов
-- ---------------------------------

-- Эффект для чтения из окружения
data Reader e next = Reader (e -> next)

instance Functor (Reader e) where
  fmap f (Reader g) = Reader (f . g)

-- Умный конструктор для Reader
ask :: Free (Reader e) e
ask = liftF (Reader id)

-- Функция для запуска Reader
runReader :: e -> Free (Reader e) a -> a
runReader e (Pure a) = a
runReader e (Free (Reader f)) = runReader e (f e)

-- Эффект для работы с состоянием
data State s next = State (s -> (next, s))

instance Functor (State s) where
  fmap f (State g) = State (\s -> let (a, s') = g s in (f a, s'))

-- Умные конструкторы для State
get :: Free (State s) s
get = liftF (State (\s -> (s, s)))

put :: s -> Free (State s) ()
put s = liftF (State (\_ -> ((), s)))

modify :: (s -> s) -> Free (State s) ()
modify f = get >>= put . f

-- Функция для запуска State
runState :: s -> Free (State s) a -> (a, s)
runState s (Pure a) = (a, s)
runState s (Free (State f)) = let (a, s') = f s in runState s' a

-- Эффект для записи логов
data Writer w next = Writer w next

instance Functor (Writer w) where
  fmap f (Writer w a) = Writer w (f a)

-- Умный конструктор для Writer
tell :: Monoid w => w -> Free (Writer w) ()
tell w = liftF (Writer w ())

-- Функция для запуска Writer
runWriter :: Monoid w => Free (Writer w) a -> (a, w)
runWriter (Pure a) = (a, mempty)
runWriter (Free (Writer w next)) = let (a, w') = runWriter next in (a, w `mappend` w')

-- Часть 4: Комбинирование эффектов
-- ----------------------------

-- Для комбинирования эффектов в реальных приложениях обычно используются
-- более сложные техники, такие как трансформеры монад или фреймворки типа freer-effects.

-- Определение трансформеров монад для примеров
newtype ReaderT e m a = ReaderT { runReaderT :: e -> m a }

instance Functor m => Functor (ReaderT e m) where
  fmap f (ReaderT g) = ReaderT (fmap f . g)

instance Applicative m => Applicative (ReaderT e m) where
  pure a = ReaderT (\_ -> pure a)
  ReaderT f <*> ReaderT g = ReaderT (\e -> f e <*> g e)

instance Monad m => Monad (ReaderT e m) where
  ReaderT f >>= g = ReaderT (\e -> f e >>= \a -> runReaderT (g a) e)

-- Функции для подъема монады в трансформер
class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (ReaderT e) where
  lift m = ReaderT $ \_ -> m

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT $ do
    a <- m
    return (a, mempty)

-- Умные конструкторы для трансформеров
askT :: Monad m => ReaderT e m e
askT = ReaderT return

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT (fmap (\(a, s) -> (f a, s)) . g)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> return (a, s))
  StateT f <*> StateT g = StateT (\s -> f s >>= \(f', s') -> g s' >>= \(a, s'') -> return (f' a, s''))

instance Monad m => Monad (StateT s m) where
  StateT f >>= g = StateT (\s -> f s >>= \(a, s') -> runStateT (g a) s')

getT :: Monad m => StateT s m s
getT = StateT (\s -> return (s, s))

putT :: Monad m => s -> StateT s m ()
putT s = StateT (\_ -> return ((), s))

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

instance Functor m => Functor (WriterT w m) where
  fmap f (WriterT g) = WriterT (fmap (\(a, w) -> (f a, w)) g)

instance (Applicative m, Monoid w) => Applicative (WriterT w m) where
  pure a = WriterT (pure (a, mempty))
  WriterT f <*> WriterT g = WriterT (liftA2 (\(f', w1) (a, w2) -> (f' a, w1 `mappend` w2)) f g)

instance (Monad m, Monoid w) => Monad (WriterT w m) where
  WriterT ma >>= f = WriterT $ do
    (a, w1) <- ma
    (b, w2) <- runWriterT (f a)
    return (b, w1 `mappend` w2)

tellT :: (Monad m, Monoid w) => w -> WriterT w m ()
tellT w = WriterT (return ((), w))

-- Пример программы с трансформерами монад
program1 :: ReaderT Int (StateT Bool (WriterT String Identity)) Int
program1 = do
  n <- askT
  s <- lift getT
  lift $ lift $ tellT $ "Got " ++ show n
  if s
    then do
      lift $ putT False
      lift $ lift $ tellT "Changed state to False"
      return (n * 2)
    else do
      lift $ lift $ tellT "State is already False"
      return n

-- Запуск program1
runProgram1 :: Int -> Bool -> (Int, (Bool, String))
runProgram1 n s = 
  let ((result, finalState), log) = runIdentity $ runWriterT $ runStateT (runReaderT program1 n) s
  in (result, (finalState, log))

-- Часть 5: Расширяемые эффекты в реальных приложениях
-- ----------------------------------------------

{-
В реальных приложениях для работы с расширяемыми эффектами обычно используются
библиотеки, такие как:

1. freer-effects: https://hackage.haskell.org/package/freer-effects
2. polysemy: https://hackage.haskell.org/package/polysemy
3. fused-effects: https://hackage.haskell.org/package/fused-effects

Эти библиотеки предоставляют более мощные и гибкие инструменты для работы с эффектами,
чем наша упрощенная реализация.

Пример использования freer-effects:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer

program :: (Member (Reader Int) r, Member (Writer String) r, Member (State Bool) r, Member (Error String) r) => Eff r Int
program = do
  n <- ask
  tell $ "Got " ++ show n
  s <- get
  if s
    then do
      put False
      return (n * 2)
    else throwError "State is False"

runProgram :: Int -> Bool -> (Either String Int, (Bool, [String]))
runProgram n s = run $ runWriter $ runState s $ runError $ runReader n program
```
-}

-- Часть 6: Продвинутые техники с расширяемыми эффектами
-- -------------------------------------------------

-- Локальное изменение окружения
local :: (e -> e) -> ReaderT e m a -> ReaderT e m a
local f m = ReaderT $ \e -> runReaderT m (f e)

-- Транзакции для State
transaction :: Monad m => StateT s m a -> StateT s m a
transaction m = do
  s <- getT
  a <- m
  putT s
  return a

-- Часть 7: Сравнение с трансформерами монад
-- --------------------------------------

{-
Сравним реализацию программы с использованием трансформеров монад и расширяемых эффектов:

-- С использованием трансформеров монад
programMT :: ReaderT Int (StateT Bool (WriterT String Identity)) Int
programMT = do
  n <- ask
  lift $ lift $ tell $ "Got " ++ show n
  s <- lift get
  if s
    then do
      lift $ put False
      return (n * 2)
    else do
      lift $ lift $ tell "State is False"
      return n

-- С использованием расширяемых эффектов (freer-effects)
programEff :: (Member (Reader Int) r, Member (Writer String) r, Member (State Bool) r) => Eff r Int
programEff = do
  n <- ask
  tell $ "Got " ++ show n
  s <- get
  if s
    then do
      put False
      return (n * 2)
    else do
      tell "State is False"
      return n

Основные различия:
1. Типы: в трансформерах монад порядок эффектов фиксирован в типе, в расширяемых эффектах - нет
2. Композиция: в трансформерах монад нужно использовать lift для подъема эффектов, в расширяемых эффектах - нет
3. Производительность: расширяемые эффекты могут быть более эффективными за счет оптимизаций
4. Гибкость: расширяемые эффекты позволяют добавлять и удалять эффекты независимо друг от друга
-}

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Комбинирование эффектов с трансформерами монад"
  
  let (result, (finalState, log)) = runProgram1 5 True
  putStrLn $ "Результат: " ++ show result
  putStrLn $ "Конечное состояние: " ++ show finalState
  putStrLn $ "Лог: " ++ log
  
  let (result2, (finalState2, log2)) = runProgram1 5 False
  putStrLn $ "\nРезультат (с False): " ++ show result2
  putStrLn $ "Конечное состояние: " ++ show finalState2
  putStrLn $ "Лог: " ++ log2

-- Главная функция
main :: IO ()
main = do
  putStrLn "Расширяемые эффекты в Haskell\n"
  
  example1
  
  putStrLn "\nКлючевые моменты о расширяемых эффектах:"
  putStrLn "1. Расширяемые эффекты - это альтернатива трансформерам монад для композиции эффектов"
  putStrLn "2. Они обеспечивают модульность, типобезопасность и часто более высокую производительность"
  putStrLn "3. Эффекты можно добавлять и удалять независимо друг от друга"
  putStrLn "4. Расширяемые эффекты позволяют разделить описание и интерпретацию эффектов"
  putStrLn "5. В реальных проектах обычно используются библиотеки, такие как freer-effects, polysemy и др."
  putStrLn "6. Расширяемые эффекты особенно полезны в больших проектах с множеством эффектов"

-- Конец файла
