{-
  Призмы и траверсалы в Haskell
  
  В этом файле мы рассмотрим призмы и траверсалы - обобщения линз,
  которые позволяют работать с суммами типов и коллекциями.
-}

module Main where

import qualified Data.Map as Map
import Data.Char (toUpper)

-- Определим типы данных для примеров
data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double
  deriving (Show, Eq)

data Person = Person
  { _name :: String
  , _age :: Int
  , _address :: Address
  } deriving (Show, Eq)

data Address = Address
  { _street :: String
  , _city :: String
  , _zipCode :: String
  } deriving (Show, Eq)

data Company = Company
  { _companyName :: String
  , _employees :: [Person]
  , _departments :: Map.Map String [Person]
  } deriving (Show, Eq)

-- Пример данных
samplePerson :: Person
samplePerson = Person
  { _name = "Иван Иванов"
  , _age = 30
  , _address = Address
      { _street = "ул. Ленина, 10"
      , _city = "Москва"
      , _zipCode = "123456"
      }
  }

sampleCompany :: Company
sampleCompany = Company
  { _companyName = "ООО Рога и Копыта"
  , _employees =
      [ samplePerson
      , Person "Мария Петрова" 28 (Address "ул. Гагарина, 5" "Москва" "123457")
      , Person "Алексей Сидоров" 35 (Address "ул. Пушкина, 15" "Санкт-Петербург" "198765")
      ]
  , _departments = Map.fromList
      [ ("Разработка", [samplePerson])
      , ("Маркетинг", [Person "Мария Петрова" 28 (Address "ул. Гагарина, 5" "Москва" "123457")])
      , ("Продажи", [Person "Алексей Сидоров" 35 (Address "ул. Пушкина, 15" "Санкт-Петербург" "198765")])
      ]
  }

sampleShapes :: [Shape]
sampleShapes =
  [ Circle 5.0
  , Rectangle 4.0 6.0
  , Triangle 3.0 4.0 5.0
  , Circle 2.5
  , Rectangle 2.0 3.0
  ]

-- Пример 1: Работа с суммами типов (призмы)
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Работа с суммами типов (призмы)"
  
  -- Функции для работы с Shape
  let area :: Shape -> Double
      area (Circle r) = pi * r * r
      area (Rectangle w h) = w * h
      area (Triangle a b c) = 
        let s = (a + b + c) / 2
        in sqrt (s * (s - a) * (s - b) * (s - c))
  
  let perimeter :: Shape -> Double
      perimeter (Circle r) = 2 * pi * r
      perimeter (Rectangle w h) = 2 * (w + h)
      perimeter (Triangle a b c) = a + b + c
  
  -- Функции для работы с конкретными типами фигур
  let isCircle :: Shape -> Bool
      isCircle (Circle _) = True
      isCircle _ = False
  
  let isRectangle :: Shape -> Bool
      isRectangle (Rectangle _ _) = True
      isRectangle _ = False
  
  let isTriangle :: Shape -> Bool
      isTriangle (Triangle _ _ _) = True
      isTriangle _ = False
  
  let getCircleRadius :: Shape -> Maybe Double
      getCircleRadius (Circle r) = Just r
      getCircleRadius _ = Nothing
  
  let getRectangleDimensions :: Shape -> Maybe (Double, Double)
      getRectangleDimensions (Rectangle w h) = Just (w, h)
      getRectangleDimensions _ = Nothing
  
  let getTriangleSides :: Shape -> Maybe (Double, Double, Double)
      getTriangleSides (Triangle a b c) = Just (a, b, c)
      getTriangleSides _ = Nothing
  
  -- Вывод информации о фигурах
  putStrLn "Информация о фигурах:"
  mapM_ (\shape -> do
    putStrLn $ "Фигура: " ++ show shape
    putStrLn $ "  Площадь: " ++ show (area shape)
    putStrLn $ "  Периметр: " ++ show (perimeter shape)
    
    case getCircleRadius shape of
      Just r -> putStrLn $ "  Тип: Круг, радиус: " ++ show r
      Nothing -> return ()
    
    case getRectangleDimensions shape of
      Just (w, h) -> putStrLn $ "  Тип: Прямоугольник, ширина: " ++ show w ++ ", высота: " ++ show h
      Nothing -> return ()
    
    case getTriangleSides shape of
      Just (a, b, c) -> putStrLn $ "  Тип: Треугольник, стороны: " ++ show a ++ ", " ++ show b ++ ", " ++ show c
      Nothing -> return ()
    
    putStrLn "") sampleShapes
  
  -- Фильтрация фигур по типу
  let circles = filter isCircle sampleShapes
  let rectangles = filter isRectangle sampleShapes
  let triangles = filter isTriangle sampleShapes
  
  putStrLn $ "Количество кругов: " ++ show (length circles)
  putStrLn $ "Количество прямоугольников: " ++ show (length rectangles)
  putStrLn $ "Количество треугольников: " ++ show (length triangles)
  
  -- Работа с конкретными типами фигур
  putStrLn "\nРадиусы всех кругов:"
  mapM_ (\shape -> case getCircleRadius shape of
    Just r -> putStrLn $ "  Радиус: " ++ show r
    Nothing -> return ()) sampleShapes
  
  putStrLn "\nРазмеры всех прямоугольников:"
  mapM_ (\shape -> case getRectangleDimensions shape of
    Just (w, h) -> putStrLn $ "  Ширина: " ++ show w ++ ", высота: " ++ show h
    Nothing -> return ()) sampleShapes

-- Пример 2: Работа с коллекциями (траверсалы)
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Работа с коллекциями (траверсалы)"
  
  -- Доступ ко всем сотрудникам компании
  putStrLn "Имена всех сотрудников:"
  mapM_ (\person -> putStrLn $ "  " ++ _name person) (_employees sampleCompany)
  
  -- Обновление всех сотрудников
  let olderCompany = sampleCompany {
        _employees = map (\person -> person { _age = _age person + 1 }) (_employees sampleCompany)
      }
  
  putStrLn "\nВозраст сотрудников после обновления:"
  mapM_ (\person -> putStrLn $ "  " ++ _name person ++ ": " ++ show (_age person)) 
        (_employees olderCompany)
  
  -- Фильтрация сотрудников
  let youngEmployees = filter (\person -> _age person < 30) (_employees sampleCompany)
  
  putStrLn "\nМолодые сотрудники (до 30 лет):"
  mapM_ (\person -> putStrLn $ "  " ++ _name person ++ ": " ++ show (_age person)) 
        youngEmployees
  
  -- Доступ к сотрудникам в отделах
  putStrLn "\nСотрудники по отделам:"
  mapM_ (\(dept, persons) -> do
    putStrLn $ "  Отдел: " ++ dept
    mapM_ (\person -> putStrLn $ "    " ++ _name person) persons)
    (Map.toList (_departments sampleCompany))
  
  -- Обновление всех сотрудников во всех отделах
  let updatedCompany = sampleCompany {
        _departments = Map.map (map (\person -> person { _name = map toUpper (_name person) })) 
                      (_departments sampleCompany)
      }
  
  putStrLn "\nИмена сотрудников в отделах после обновления:"
  mapM_ (\(dept, persons) -> do
    putStrLn $ "  Отдел: " ++ dept
    mapM_ (\person -> putStrLn $ "    " ++ _name person) persons)
    (Map.toList (_departments updatedCompany))

-- Пример 3: Комбинирование призм и траверсалов
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Комбинирование призм и траверсалов"
  
  -- Функции для работы с конкретными типами фигур
  let isCircle :: Shape -> Bool
      isCircle (Circle _) = True
      isCircle _ = False
  
  let isRectangle :: Shape -> Bool
      isRectangle (Rectangle _ _) = True
      isRectangle _ = False
  
  let isTriangle :: Shape -> Bool
      isTriangle (Triangle _ _ _) = True
      isTriangle _ = False
  
  -- Функция для получения площади фигуры
  let shapeArea :: Shape -> Double
      shapeArea (Circle r) = pi * r * r
      shapeArea (Rectangle w h) = w * h
      shapeArea (Triangle a b c) = 
        let s = (a + b + c) / 2
        in sqrt (s * (s - a) * (s - b) * (s - c))
  
  -- Вычисление общей площади всех фигур
  let totalArea = sum (map shapeArea sampleShapes)
  putStrLn $ "Общая площадь всех фигур: " ++ show totalArea
  
  -- Вычисление общей площади по типам фигур
  let circlesArea = sum (map shapeArea (filter isCircle sampleShapes))
  let rectanglesArea = sum (map shapeArea (filter isRectangle sampleShapes))
  let trianglesArea = sum (map shapeArea (filter isTriangle sampleShapes))
  
  putStrLn $ "Общая площадь кругов: " ++ show circlesArea
  putStrLn $ "Общая площадь прямоугольников: " ++ show rectanglesArea
  putStrLn $ "Общая площадь треугольников: " ++ show trianglesArea
  
  -- Функция для увеличения размеров фигуры
  let scaleShape :: Double -> Shape -> Shape
      scaleShape factor (Circle r) = Circle (r * factor)
      scaleShape factor (Rectangle w h) = Rectangle (w * factor) (h * factor)
      scaleShape factor (Triangle a b c) = Triangle (a * factor) (b * factor) (c * factor)
  
  -- Увеличение всех фигур в два раза
  let scaledShapes = map (scaleShape 2.0) sampleShapes
  
  putStrLn "\nПлощади фигур после увеличения в два раза:"
  mapM_ (\shape -> putStrLn $ "  " ++ show shape ++ ": " ++ show (shapeArea shape)) 
        scaledShapes
  
  -- Общая площадь после увеличения
  let totalScaledArea = sum (map shapeArea scaledShapes)
  putStrLn $ "Общая площадь после увеличения: " ++ show totalScaledArea
  putStrLn $ "Отношение новой площади к старой: " ++ show (totalScaledArea / totalArea)

-- Пример 4: Практическое применение призм и траверсалов
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Практическое применение призм и траверсалов"
  
  -- Функция для форматирования адреса
  let formatAddress :: Address -> String
      formatAddress addr = _street addr ++ ", " ++ _city addr ++ ", " ++ _zipCode addr
  
  -- Получение адресов всех сотрудников
  putStrLn "Адреса всех сотрудников:"
  mapM_ (\person -> putStrLn $ "  " ++ _name person ++ ": " ++ formatAddress (_address person)) 
        (_employees sampleCompany)
  
  -- Группировка сотрудников по городам
  let employeesByCity = Map.fromListWith (++) $
        map (\person -> (_city (_address person), [person])) (_employees sampleCompany)
  
  putStrLn "\nСотрудники по городам:"
  mapM_ (\(city, persons) -> do
    putStrLn $ "  Город: " ++ city
    mapM_ (\person -> putStrLn $ "    " ++ _name person) persons)
    (Map.toList employeesByCity)
  
  -- Обновление почтовых индексов для всех сотрудников из Москвы
  let updateZipCode person =
        if _city (_address person) == "Москва"
        then person { _address = (_address person) { _zipCode = "101000" } }
        else person
  
  let updatedCompany = sampleCompany {
        _employees = map updateZipCode (_employees sampleCompany)
      }
  
  putStrLn "\nАдреса сотрудников после обновления почтовых индексов:"
  mapM_ (\person -> putStrLn $ "  " ++ _name person ++ ": " ++ formatAddress (_address person)) 
        (_employees updatedCompany)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Призмы и траверсалы в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о призмах и траверсалах:"
  putStrLn "1. Призмы - это обобщение линз для работы с суммами типов (Either, Maybe, ADT)"
  putStrLn "2. Траверсалы - это обобщение линз для работы с коллекциями (списки, Map, Set)"
  putStrLn "3. Призмы и траверсалы можно комбинировать для работы со сложными структурами данных"
  putStrLn "4. Призмы и траверсалы делают код более декларативным и выразительным"
  putStrLn "5. В реальных проектах призмы и траверсалы часто используются вместе с линзами"
