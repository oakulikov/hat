{-
  Создание собственных функторов в Haskell
  
  В этом файле мы рассмотрим, как создавать собственные типы данных,
  которые являются функторами, и как реализовывать для них экземпляры
  типового класса Functor.
-}

module CustomFunctors where

-- Напомним определение типового класса Functor:
-- 
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- Пример 1: Простой контейнер - Box
-- Создадим простой контейнер, который может содержать значение или быть пустым
data Box a = Empty | Box a
  deriving (Show, Eq)

-- Реализация Functor для Box
instance Functor Box where
  -- Для пустого контейнера результат тоже пустой
  fmap _ Empty = Empty
  -- Для непустого контейнера применяем функцию к значению
  fmap f (Box x) = Box (f x)

boxExample :: IO ()
boxExample = do
  putStrLn "1. Функтор Box:"
  
  let box1 = Box 42
  let box2 = Empty :: Box Int
  
  putStrLn $ "  Исходные значения: " ++ show box1 ++ ", " ++ show box2
  
  let doubledBox1 = fmap (*2) box1
  let doubledBox2 = fmap (*2) box2
  
  putStrLn $ "  После fmap (*2): " ++ show doubledBox1 ++ ", " ++ show doubledBox2
  
  -- Цепочка преобразований
  let result = fmap show (fmap (+10) box1)
  putStrLn $ "  Цепочка преобразований: " ++ show result

-- Пример 2: Бинарное дерево
-- Создадим тип данных для бинарного дерева
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Реализация Functor для Tree
instance Functor Tree where
  -- Для пустого дерева (листа) результат тоже пустой
  fmap _ Leaf = Leaf
  -- Для узла применяем функцию к значению и рекурсивно к поддеревьям
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

-- Вспомогательная функция для создания дерева
sampleTree :: Tree Int
sampleTree = Node 1
               (Node 2 Leaf Leaf)
               (Node 3
                 (Node 4 Leaf Leaf)
                 Leaf)

-- Функция для красивого вывода дерева
prettyPrintTree :: Show a => Tree a -> IO ()
prettyPrintTree tree = putStrLn $ drawTree "" tree
  where
    drawTree prefix Leaf = prefix ++ "└── (empty)"
    drawTree prefix (Node x left right) =
      prefix ++ "└── " ++ show x ++ "\n" ++
      prefix ++ "    " ++ drawTree (prefix ++ "    ") left ++ "\n" ++
      prefix ++ "    " ++ drawTree (prefix ++ "    ") right

treeExample :: IO ()
treeExample = do
  putStrLn "\n2. Функтор Tree:"
  
  putStrLn "  Исходное дерево:"
  prettyPrintTree sampleTree
  
  putStrLn "  После fmap (*2):"
  prettyPrintTree (fmap (*2) sampleTree)
  
  putStrLn "  После fmap show:"
  prettyPrintTree (fmap show sampleTree)

-- Пример 3: Функтор для пары с фиксированным первым элементом
-- Напомним, что стандартный функтор для пары (,) применяет функцию только ко второму элементу
-- Мы можем создать свой тип данных для пары и реализовать функтор по-другому

-- Пара с возможностью применения функции к обоим элементам
data BothPair a b = BothPair a b
  deriving (Show, Eq)

-- Функтор по второму элементу (как стандартный)
instance Functor (BothPair a) where
  fmap f (BothPair a b) = BothPair a (f b)

-- Функция для применения функции к первому элементу
mapFirst :: (a -> c) -> BothPair a b -> BothPair c b
mapFirst f (BothPair a b) = BothPair (f a) b

-- Функция для применения функций к обоим элементам
mapBoth :: (a -> c) -> (b -> d) -> BothPair a b -> BothPair c d
mapBoth f g (BothPair a b) = BothPair (f a) (g b)

pairExample :: IO ()
pairExample = do
  putStrLn "\n3. Функтор для пары:"
  
  let pair = BothPair "hello" 42
  putStrLn $ "  Исходная пара: " ++ show pair
  
  -- Применение функции ко второму элементу (стандартное поведение функтора)
  let mappedSecond = fmap (*2) pair
  putStrLn $ "  fmap (*2): " ++ show mappedSecond
  
  -- Применение функции к первому элементу
  let mappedFirst = mapFirst reverse pair
  putStrLn $ "  mapFirst reverse: " ++ show mappedFirst
  
  -- Применение функций к обоим элементам
  let mappedBoth = mapBoth reverse (*2) pair
  putStrLn $ "  mapBoth reverse (*2): " ++ show mappedBoth

-- Пример 4: Функтор для функции с фиксированным аргументом
-- Напомним, что стандартный функтор для функции ((->) r) - это композиция функций
-- Мы можем создать свой тип данных для функции и реализовать функтор

-- Обертка для функции
newtype MyFunc r a = MyFunc { runFunc :: r -> a }

-- Функтор для MyFunc
instance Functor (MyFunc r) where
  fmap f (MyFunc g) = MyFunc (f . g)

funcExample :: IO ()
funcExample = do
  putStrLn "\n4. Функтор для функции:"
  
  -- Создаем функцию
  let addOne = MyFunc (+1)
  
  -- Применяем функцию к аргументу
  putStrLn $ "  addOne 5 = " ++ show (runFunc addOne 5)
  
  -- Применяем функтор
  let doubledAddOne = fmap (*2) addOne
  putStrLn $ "  fmap (*2) addOne = " ++ show (runFunc doubledAddOne 5)
  
  -- Эквивалентно композиции функций
  putStrLn $ "  Эквивалентно: (*2) . (+1) = " ++ show (((*2) . (+1)) 5)

-- Пример 5: Функтор для типа данных с несколькими параметрами
-- Для типов данных с несколькими параметрами нужно указать,
-- по какому параметру мы хотим реализовать функтор

-- Тип данных с двумя параметрами
data Result e a = Error e | Success a
  deriving (Show, Eq)

-- Функтор по второму параметру (как Either)
instance Functor (Result e) where
  fmap _ (Error e) = Error e
  fmap f (Success a) = Success (f a)

-- Функция для применения функции к ошибке
mapError :: (e -> e') -> Result e a -> Result e' a
mapError f (Error e) = Error (f e)
mapError _ (Success a) = Success a

resultExample :: IO ()
resultExample = do
  putStrLn "\n5. Функтор для типа данных с несколькими параметрами:"
  
  let success = Success 42 :: Result String Int
  let error = Error "Something went wrong" :: Result String Int
  
  putStrLn $ "  Исходные значения: " ++ show success ++ ", " ++ show error
  
  -- Применение функтора к успешному результату
  let doubledSuccess = fmap (*2) success
  putStrLn $ "  fmap (*2) success = " ++ show doubledSuccess
  
  -- Применение функтора к ошибке
  let doubledError = fmap (*2) error
  putStrLn $ "  fmap (*2) error = " ++ show doubledError
  
  -- Применение функции к ошибке
  let uppercasedError = mapError (map toUpper) error
  putStrLn $ "  mapError (map toUpper) error = " ++ show uppercasedError

-- Пример 6: Функтор для вложенных типов данных
-- Мы можем создать функтор для типа данных, который содержит другой функтор

-- Тип данных, который содержит два значения в контексте функтора f
data Paired f a = Paired (f a) (f a)
  deriving (Show, Eq)

-- Функтор для Paired, если f тоже функтор
instance Functor f => Functor (Paired f) where
  fmap f (Paired x y) = Paired (fmap f x) (fmap f y)

pairedExample :: IO ()
pairedExample = do
  putStrLn "\n6. Функтор для вложенных типов данных:"
  
  -- Создаем значение типа Paired Maybe Int
  let paired = Paired (Just 10) (Just 20)
  putStrLn $ "  Исходное значение: " ++ show paired
  
  -- Применяем функтор
  let doubledPaired = fmap (*2) paired
  putStrLn $ "  fmap (*2) = " ++ show doubledPaired
  
  -- Создаем значение с Nothing
  let pairedWithNothing = Paired (Just 10) Nothing
  putStrLn $ "  Значение с Nothing: " ++ show pairedWithNothing
  
  -- Применяем функтор
  let doubledPairedWithNothing = fmap (*2) pairedWithNothing
  putStrLn $ "  fmap (*2) = " ++ show doubledPairedWithNothing

-- Пример 7: Функтор для типа данных с ограничениями
-- Иногда мы хотим создать функтор, который работает только с определенными типами

-- Тип данных для сортированного списка
newtype Sorted a = Sorted [a]
  deriving (Show, Eq)

-- Функция для создания сортированного списка
mkSorted :: Ord a => [a] -> Sorted a
mkSorted xs = Sorted (sort xs)
  where
    sort [] = []
    sort (x:xs) = sort [y | y <- xs, y <= x] ++ [x] ++ sort [y | y <- xs, y > x]

-- Функтор для Sorted, который работает только с типами, поддерживающими сравнение
instance Functor Sorted where
  -- Мы не можем гарантировать, что результат будет отсортирован,
  -- поэтому этот функтор не соблюдает законы функторов!
  fmap f (Sorted xs) = Sorted (map f xs)

-- Правильная версия fmap, которая сохраняет инвариант сортировки
fmapSorted :: Ord b => (a -> b) -> Sorted a -> Sorted b
fmapSorted f (Sorted xs) = mkSorted (map f xs)

sortedExample :: IO ()
sortedExample = do
  putStrLn "\n7. Функтор с ограничениями:"
  
  let sorted = mkSorted [3, 1, 4, 1, 5, 9, 2, 6, 5]
  putStrLn $ "  Исходный отсортированный список: " ++ show sorted
  
  -- Применяем обычный fmap (может нарушить сортировку)
  let negated = fmap negate sorted
  putStrLn $ "  fmap negate (нарушает сортировку): " ++ show negated
  
  -- Применяем правильную версию fmap
  let negatedSorted = fmapSorted negate sorted
  putStrLn $ "  fmapSorted negate (сохраняет сортировку): " ++ show negatedSorted
  
  putStrLn "  Заметьте, что обычный fmap может нарушить инвариант сортировки,"
  putStrLn "  поэтому такая реализация функтора не соблюдает законы функторов!"

-- Вспомогательная функция для преобразования символа в верхний регистр
toUpper :: Char -> Char
toUpper c
  | c >= 'a' && c <= 'z' = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
  | otherwise = c

-- Главная функция
main :: IO ()
main = do
  putStrLn "Создание собственных функторов в Haskell\n"
  
  boxExample
  treeExample
  pairExample
  funcExample
  resultExample
  pairedExample
  sortedExample
  
  putStrLn "\nКлючевые моменты о создании собственных функторов:"
  putStrLn "1. Функтор можно реализовать для любого типа данных с параметром типа"
  putStrLn "2. Реализация должна соблюдать законы функторов"
  putStrLn "3. Для типов с несколькими параметрами нужно указать, по какому параметру реализуется функтор"
  putStrLn "4. Можно создавать функторы для типов, которые содержат другие функторы"
  putStrLn "5. Иногда требуются дополнительные ограничения на типы"
  putStrLn "6. Важно сохранять инварианты типа данных при реализации функтора"
