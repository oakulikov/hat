
{-
  Монадические морфизмы в Haskell
  
  В этом файле мы рассмотрим монадические морфизмы - преобразования
  между монадами и их применение в функциональном программировании.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad
import Control.Applicative (liftA2)
import Data.Functor.Identity
import Data.Char (toUpper)

-- Часть 1: Введение в монадические морфизмы
-- --------------------------------------

{-
Монадический морфизм - это функция, которая преобразует вычисления из одной монады в другую,
сохраняя монадическую структуру. Формально, монадический морфизм между монадами m и n - это
функция типа:

    forall a. m a -> n a

которая удовлетворяет следующим законам:

1. morphism . return = return
   (морфизм сохраняет pure/return)

2. morphism (m >>= f) = morphism m >>= (morphism . f)
   (морфизм сохраняет bind)

Монадические морфизмы полезны для:
- Преобразования между различными представлениями эффектов
- Композиции монад
- Оптимизации монадических вычислений
- Тестирования монадических программ
-}

-- Определение типа для монадического морфизма
type MonadMorphism m n = forall a. m a -> n a

-- Часть 2: Примеры монадических морфизмов
-- ------------------------------------

-- Пример 1: Identity -> m (внедрение значения в монаду)
idToM :: Monad m => Identity a -> m a
idToM (Identity a) = return a

-- Пример 2: Maybe -> Either e (преобразование Maybe в Either)
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a

-- Пример 3: Either e -> Maybe (преобразование Either в Maybe)
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

-- Пример 4: [] -> Maybe (преобразование списка в Maybe)
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

-- Часть 3: Определение трансформеров монад
-- ------------------------------------

-- Определение класса MonadTrans
class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- Определение ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT g) = ReaderT (fmap f . g)

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT (\_ -> pure a)
  ReaderT f <*> ReaderT g = ReaderT (\e -> f e <*> g e)

instance Monad m => Monad (ReaderT r m) where
  ReaderT f >>= g = ReaderT (\e -> f e >>= \a -> runReaderT (g a) e)

instance MonadTrans (ReaderT r) where
  lift m = ReaderT (\_ -> m)

-- Определение WriterT
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

instance Monoid w => MonadTrans (WriterT w) where
  lift m = WriterT $ do
    a <- m
    return (a, mempty)

-- Определение StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT g) = StateT (fmap (\(a, s) -> (f a, s)) . g)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> return (a, s))
  StateT f <*> StateT g = StateT (\s -> f s >>= \(f', s') -> g s' >>= \(a, s'') -> return (f' a, s''))

instance Monad m => Monad (StateT s m) where
  StateT f >>= g = StateT (\s -> f s >>= \(a, s') -> runStateT (g a) s')

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

-- Определение MaybeT
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT (fmap (fmap f) ma)

instance Monad m => Applicative (MaybeT m) where
  pure a = MaybeT (return (Just a))
  MaybeT mf <*> MaybeT ma = MaybeT $ do
    mf' <- mf
    case mf' of
      Nothing -> return Nothing
      Just f -> do
        ma' <- ma
        return (f <$> ma')

instance Monad m => Monad (MaybeT m) where
  MaybeT ma >>= f = MaybeT $ do
    a <- ma
    case a of
      Nothing -> return Nothing
      Just a' -> runMaybeT (f a')

instance MonadTrans MaybeT where
  lift m = MaybeT (fmap Just m)

-- Определение ExceptT
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
  fmap f (ExceptT ma) = ExceptT (fmap (fmap f) ma)

instance Monad m => Applicative (ExceptT e m) where
  pure a = ExceptT (return (Right a))
  ExceptT mf <*> ExceptT ma = ExceptT $ do
    mf' <- mf
    case mf' of
      Left e -> return (Left e)
      Right f -> do
        ma' <- ma
        return (f <$> ma')

instance Monad m => Monad (ExceptT e m) where
  ExceptT ma >>= f = ExceptT $ do
    a <- ma
    case a of
      Left e -> return (Left e)
      Right a' -> runExceptT (f a')

instance MonadTrans (ExceptT e) where
  lift m = ExceptT (fmap Right m)

-- Определение IdentityT
newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT (fmap f ma)

instance Applicative m => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)
  IdentityT mf <*> IdentityT ma = IdentityT (mf <*> ma)

instance Monad m => Monad (IdentityT m) where
  IdentityT ma >>= f = IdentityT (ma >>= runIdentityT . f)

instance MonadTrans IdentityT where
  lift = IdentityT

-- Определение Writer и State (для удобства)
type Writer w = WriterT w Identity
type State s = StateT s Identity

-- Функции для запуска Writer и State
runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

runState :: State s a -> s -> (a, s)
runState m s = runIdentity (runStateT m s)

-- Часть 4: Монадические морфизмы для трансформеров монад
-- -------------------------------------------------

-- Опускание (lower) - это монадический морфизм из трансформера в базовую монаду

-- Опускание для MaybeT
lowerMaybeT :: Monad m => MaybeT m a -> m (Maybe a)
lowerMaybeT = runMaybeT

-- Опускание для ExceptT
lowerExceptT :: Monad m => ExceptT e m a -> m (Either e a)
lowerExceptT = runExceptT

-- Опускание для ReaderT
lowerReaderT :: r -> ReaderT r m a -> m a
lowerReaderT r = flip runReaderT r

-- Опускание для WriterT
lowerWriterT :: Monad m => WriterT w m a -> m (a, w)
lowerWriterT = runWriterT

-- Опускание для StateT
lowerStateT :: Monad m => s -> StateT s m a -> m (a, s)
lowerStateT s = flip runStateT s

-- Часть 5: Композиция монадических морфизмов
-- --------------------------------------

-- Композиция монадических морфизмов
composeM :: (Monad m, Monad n, Monad p) => (forall a. n a -> p a) -> (forall a. m a -> n a) -> (forall a. m a -> p a)
composeM f g = f . g

-- Пример композиции: Maybe -> Either e -> IO
maybeToIO :: e -> Maybe a -> IO (Either e a)
maybeToIO e = return . maybeToEither e

-- Часть 6: Монадические морфизмы и естественные трансформации
-- ------------------------------------------------------

{-
Монадические морфизмы тесно связаны с понятием естественных трансформаций из теории категорий.
Естественная трансформация между функторами F и G - это семейство морфизмов η_a: F a -> G a,
которые удовлетворяют условию естественности:

    η_b . F f = G f . η_a

для любой функции f: a -> b.

Монадический морфизм - это естественная трансформация, которая дополнительно сохраняет
монадическую структуру (законы для return и bind).
-}

-- Определение типа для естественной трансформации
type NaturalTransformation f g = forall a. f a -> g a

-- Проверка условия естественности (для иллюстрации)
isNatural :: (Functor f, Functor g, Eq (g b)) => NaturalTransformation f g -> (a -> b) -> f a -> Bool
isNatural eta f fa = eta (fmap f fa) == fmap f (eta fa)

-- Часть 7: Применение монадических морфизмов
-- --------------------------------------

-- Пример 1: Тестирование IO программ
-- Монадический морфизм из IO в Identity для тестирования
type IOAction a = IO a
type PureAction a = Identity a

-- Мок-функция для чтения из файла
mockReadFile :: FilePath -> PureAction String
mockReadFile path = Identity $ "Mock content for " ++ path

-- Мок-функция для записи в файл
mockWriteFile :: FilePath -> String -> PureAction ()
mockWriteFile _ _ = Identity ()

-- Реальная программа с IO
realProgram :: FilePath -> FilePath -> IOAction ()
realProgram inFile outFile = do
  content <- readFile inFile
  let processed = map toUpper content
  writeFile outFile processed

-- Тестовая версия программы
testProgram :: FilePath -> FilePath -> PureAction ()
testProgram inFile outFile = do
  content <- mockReadFile inFile
  let processed = map toUpper content
  mockWriteFile outFile processed

-- Пример 2: Преобразование между различными представлениями эффектов
-- Программа с использованием Maybe
maybeProgram :: Int -> Maybe Int
maybeProgram n = do
  x <- if n > 0 then Just n else Nothing
  return (x * 2)

-- Преобразование в Either для более информативных ошибок
eitherProgram :: Int -> Either String Int
eitherProgram n = maybeToEither "Non-positive number" $ maybeProgram n

-- Пример 3: Оптимизация монадических вычислений
-- Оптимизация вложенных StateT трансформеров
type NestedState s1 s2 a = StateT s1 (State s2) a

-- Монадический морфизм для объединения вложенных состояний
optimizeNestedState :: NestedState s1 s2 a -> State (s1, s2) a
optimizeNestedState m = StateT $ \(s1, s2) ->
  let ((a, s1'), s2') = runState (runStateT m s1) s2
  in Identity (a, (s1', s2'))

-- Пример 4: Реализация эффектов с помощью монадических морфизмов
-- Реализация Reader с помощью State
readerToState :: ReaderT r Identity a -> StateT r Identity a
readerToState m = StateT $ \s -> Identity (runIdentity (runReaderT m s), s)

-- Реализация Writer с помощью State
writerToState :: (Monoid w) => WriterT w Identity a -> StateT w Identity a
writerToState m = StateT $ \s ->
  let (a, w) = runIdentity (runWriterT m)
  in Identity (a, s `mappend` w)

-- Часть 8: Продвинутые примеры монадических морфизмов
-- -----------------------------------------------

-- Пример 1: Монадический морфизм для Free монады
data Free f a = Pure a | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free fa) = Free (fmap (fmap f) fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> a = fmap f a
  Free ff <*> a = Free (fmap (<*> a) ff)

instance Functor f => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free fa >>= f = Free (fmap (>>= f) fa)

-- Интерпретация Free монады в другую монаду
foldFree :: (Functor f, Monad m) => (forall x. f x -> m x) -> Free f a -> m a
foldFree _ (Pure a) = return a
foldFree f (Free fx) = f fx >>= foldFree f

-- Пример 2: Монадический морфизм для Codensity монады
newtype Codensity m a = Codensity { runCodensity :: forall b. (a -> m b) -> m b }

-- Преобразование из Codensity в базовую монаду
lowerCodensity :: Monad m => Codensity m a -> m a
lowerCodensity (Codensity f) = f return

-- Преобразование из базовой монады в Codensity
toCodensity :: Monad m => m a -> Codensity m a
toCodensity m = Codensity (m >>=)

-- Часть 9: Законы монадических морфизмов
-- ----------------------------------

-- Проверка закона сохранения return
checkReturnLaw :: (Monad m, Monad n, Eq (n a), Show (n a)) => MonadMorphism m n -> a -> Bool
checkReturnLaw morph a =
  let lhs = morph (return a)
      rhs = return a
  in if lhs == rhs
     then True
     else error $ "Return law violated: " ++ show lhs ++ " /= " ++ show rhs

-- Проверка закона сохранения bind
checkBindLaw :: (Monad m, Monad n, Eq (n c), Show (n c)) => MonadMorphism m n -> m a -> (a -> m b) -> (b -> n c) -> Bool
checkBindLaw morph ma f g =
  let lhs = morph (ma >>= f) >>= g
      rhs = morph ma >>= (\a -> morph (f a) >>= g)
  in if lhs == rhs
     then True
     else error $ "Bind law violated: " ++ show lhs ++ " /= " ++ show rhs

-- Примеры для демонстрации
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Преобразование Maybe в Either"
  
  let m1 = Just 42
  let m2 = Nothing
  let e1 = maybeToEither "Error" m1 :: Either String Int
  let e2 = maybeToEither "Error" m2 :: Either String Int
  
  putStrLn $ "Just 42 -> " ++ show e1
  putStrLn $ "Nothing -> " ++ show e2

example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Преобразование между монадическими трансформерами"
  
  let r1 = runReader (ReaderT $ \n -> return (n * 2)) 21
  let r2 = runIdentity r1
  
  putStrLn $ "ReaderT (\\n -> return (n * 2)) 21 -> " ++ show r2
  where
    runReader :: ReaderT r Identity a -> r -> Identity a
    runReader = runReaderT

example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Композиция монадических морфизмов"
  
  let m = Just 42
  let e = maybeToEither "Error" m
  let result = runIdentity (return e)
  
  putStrLn $ "Just 42 -> Either -> Identity -> " ++ show result

example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Оптимизация вложенных состояний"
  
  let nestedState = do
        modify (+ 1) :: StateT Int (State String) ()
        lift $ modify (++ "!") :: StateT Int (State String) ()
        n <- get :: StateT Int (State String) Int
        s <- lift get :: StateT Int (State String) String
        return (n, s)
  
  let optimizedState = optimizeNestedState nestedState
  let result = runState optimizedState (0, "Hello")
  
  putStrLn $ "Результат оптимизированного состояния: " ++ show result
  where
    modify :: Monad m => (s -> s) -> StateT s m ()
    modify f = StateT $ \s -> return ((), f s)
    
    get :: Monad m => StateT s m s
    get = StateT $ \s -> return (s, s)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Монадические морфизмы в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о монадических морфизмах:"
  putStrLn "1. Монадический морфизм - это функция типа forall a. m a -> n a, сохраняющая монадическую структуру"
  putStrLn "2. Монадические морфизмы должны удовлетворять законам сохранения return и bind"
  putStrLn "3. Монадические морфизмы полезны для преобразования между различными представлениями эффектов"
  putStrLn "4. Монадические морфизмы связаны с естественными трансформациями из теории категорий"
  putStrLn "5. Монадические морфизмы могут использоваться для оптимизации монадических вычислений"
  putStrLn "6. Монадические морфизмы позволяют тестировать программы с эффектами в чистом окружении"
