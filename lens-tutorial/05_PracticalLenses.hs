{-
  Практическое применение линз в Haskell
  
  В этом файле мы рассмотрим практические примеры использования линз
  для решения реальных задач.
-}

module PracticalLenses where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Char (toUpper, toLower)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

-- Пример 1: Работа с конфигурационными файлами
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Работа с конфигурационными файлами"
  
  -- Определим структуру для конфигурации приложения
  let config = AppConfig
        { _appName = "MyApp"
        , _version = "1.0.0"
        , _settings = Map.fromList
            [ ("theme", "dark")
            , ("language", "ru")
            , ("fontSize", "14")
            ]
        , _paths = Paths
            { _dataPath = "/var/data"
            , _logPath = "/var/log"
            , _configPath = "/etc/myapp"
            }
        , _features = Features
            { _enableLogging = True
            , _enableCache = True
            , _debugMode = False
            }
        }
  
  -- Создадим линзы для работы с конфигурацией
  let appNameL = lens _appName (\c n -> c { _appName = n })
  let versionL = lens _version (\c v -> c { _version = v })
  let settingsL = lens _settings (\c s -> c { _settings = s })
  let pathsL = lens _paths (\c p -> c { _paths = p })
  let featuresL = lens _features (\c f -> c { _features = f })
  
  -- Создадим линзы для вложенных структур
  let dataPathL = lens _dataPath (\p d -> p { _dataPath = d })
  let logPathL = lens _logPath (\p l -> p { _logPath = l })
  let configPathL = lens _configPath (\p c -> p { _configPath = c })
  
  let enableLoggingL = lens _enableLogging (\f l -> f { _enableLogging = l })
  let enableCacheL = lens _enableCache (\f c -> f { _enableCache = c })
  let debugModeL = lens _debugMode (\f d -> f { _debugMode = d })
  
  -- Составные линзы для доступа к вложенным полям
  let appDataPathL = pathsL . dataPathL
  let appLogPathL = pathsL . logPathL
  let appDebugModeL = featuresL . debugModeL
  
  -- Получение значений из конфигурации
  putStrLn $ "Название приложения: " ++ (config ^. appNameL)
  putStrLn $ "Версия: " ++ (config ^. versionL)
  putStrLn $ "Тема: " ++ fromMaybe "не указана" (config ^. settingsL . at "theme")
  putStrLn $ "Путь к данным: " ++ (config ^. appDataPathL)
  putStrLn $ "Режим отладки: " ++ show (config ^. appDebugModeL)
  
  -- Обновление конфигурации
  let updatedConfig = config & versionL .~ "1.1.0"
                            & settingsL . at "theme" ?~ "light"
                            & appLogPathL .~ "/var/log/myapp"
                            & appDebugModeL .~ True
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Версия: " ++ (updatedConfig ^. versionL)
  putStrLn $ "Тема: " ++ fromMaybe "не указана" (updatedConfig ^. settingsL . at "theme")
  putStrLn $ "Путь к логам: " ++ (updatedConfig ^. appLogPathL)
  putStrLn $ "Режим отладки: " ++ show (updatedConfig ^. appDebugModeL)
  
  -- Создание конфигурации для разработки
  let devConfig = config & appNameL %~ (++ "-dev")
                        & settingsL . at "environment" ?~ "development"
                        & appDebugModeL .~ True
                        & pathsL . traverse %~ (++ "-dev")  -- Изменяем все пути
  
  putStrLn $ "\nКонфигурация для разработки:"
  putStrLn $ "Название приложения: " ++ (devConfig ^. appNameL)
  putStrLn $ "Окружение: " ++ fromMaybe "не указано" (devConfig ^. settingsL . at "environment")
  putStrLn $ "Путь к данным: " ++ (devConfig ^. appDataPathL)
  putStrLn $ "Путь к логам: " ++ (devConfig ^. appLogPathL)
  putStrLn $ "Режим отладки: " ++ show (devConfig ^. appDebugModeL)

-- Структуры данных для примера 1
data AppConfig = AppConfig
  { _appName :: String
  , _version :: String
  , _settings :: Map.Map String String
  , _paths :: Paths
  , _features :: Features
  } deriving (Show, Eq)

data Paths = Paths
  { _dataPath :: String
  , _logPath :: String
  , _configPath :: String
  } deriving (Show, Eq)

data Features = Features
  { _enableLogging :: Bool
  , _enableCache :: Bool
  , _debugMode :: Bool
  } deriving (Show, Eq)

-- Пример 2: Работа с данными пользователей
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Работа с данными пользователей"
  
  -- Определим структуру для пользователей
  let users = 
        [ User "user1" "Иван Иванов" "ivan@example.com" 30 
            (Address "ул. Ленина, 10" "Москва" "123456") 
            [Role "admin" True, Role "user" True]
        , User "user2" "Мария Петрова" "maria@example.com" 28 
            (Address "ул. Пушкина, 15" "Санкт-Петербург" "654321") 
            [Role "user" True]
        , User "user3" "Алексей Сидоров" "alex@example.com" 35 
            (Address "ул. Гагарина, 20" "Казань" "420001") 
            [Role "editor" True, Role "user" True]
        ]
  
  -- Создадим линзы для работы с пользователями
  let usernameL = lens _username (\u n -> u { _username = n })
  let fullNameL = lens _fullName (\u n -> u { _fullName = n })
  let emailL = lens _email (\u e -> u { _email = e })
  let ageL = lens _age (\u a -> u { _age = a })
  let addressL = lens _address (\u a -> u { _address = a })
  let rolesL = lens _roles (\u r -> u { _roles = r })
  
  -- Создадим линзы для адреса
  let streetL = lens _street (\a s -> a { _street = s })
  let cityL = lens _city (\a c -> a { _city = c })
  let zipCodeL = lens _zipCode (\a z -> a { _zipCode = z })
  
  -- Создадим линзы для ролей
  let roleNameL = lens _roleName (\r n -> r { _roleName = n })
  let activeL = lens _active (\r a -> r { _active = a })
  
  -- Составные линзы
  let userCityL = addressL . cityL
  
  -- Получение информации о пользователях
  putStrLn "Список пользователей:"
  mapM_ (\u -> putStrLn $ "- " ++ (u ^. fullNameL) ++ " (" ++ (u ^. emailL) ++ ")") users
  
  -- Фильтрация пользователей по городу
  let moscowUsers = filter (\u -> u ^. userCityL == "Москва") users
  putStrLn $ "\nПользователи из Москвы:"
  mapM_ (\u -> putStrLn $ "- " ++ (u ^. fullNameL)) moscowUsers
  
  -- Фильтрация пользователей по роли
  let admins = filter (hasRole "admin") users
        where hasRole r u = any (\role -> role ^. roleNameL == r && role ^. activeL) (u ^. rolesL)
  
  putStrLn $ "\nАдминистраторы:"
  mapM_ (\u -> putStrLn $ "- " ++ (u ^. fullNameL)) admins
  
  -- Обновление данных пользователей
  let updatedUsers = over (traverse . filtered (\u -> u ^. ageL > 30)) 
                          (emailL %~ map toUpper) users
  
  putStrLn $ "\nEmail пользователей старше 30 лет (в верхнем регистре):"
  mapM_ (\u -> putStrLn $ "- " ++ (u ^. fullNameL) ++ ": " ++ (u ^. emailL)) updatedUsers
  
  -- Добавление новой роли всем пользователям
  let usersWithNewRole = over traverse 
                              (rolesL %~ (++ [Role "viewer" True])) 
                              users
  
  putStrLn $ "\nРоли пользователя " ++ (head usersWithNewRole ^. fullNameL) ++ ":"
  mapM_ (\r -> putStrLn $ "- " ++ (r ^. roleNameL) ++ 
                          (if r ^. activeL then " (активна)" else " (неактивна)")) 
        (head usersWithNewRole ^. rolesL)

-- Структуры данных для примера 2
data User = User
  { _username :: String
  , _fullName :: String
  , _email :: String
  , _age :: Int
  , _address :: Address
  , _roles :: [Role]
  } deriving (Show, Eq)

data Address = Address
  { _street :: String
  , _city :: String
  , _zipCode :: String
  } deriving (Show, Eq)

data Role = Role
  { _roleName :: String
  , _active :: Bool
  } deriving (Show, Eq)

-- Пример 3: Работа с деревьями данных
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Работа с деревьями данных"
  
  -- Определим структуру для дерева файловой системы
  let fileSystem = Directory "root" 
        [ File "readme.txt" 1024 "text/plain"
        , Directory "docs" 
            [ File "manual.pdf" 2048 "application/pdf"
            , File "guide.docx" 1536 "application/msword"
            ]
        , Directory "src" 
            [ File "main.hs" 512 "text/x-haskell"
            , File "utils.hs" 768 "text/x-haskell"
            , Directory "lib" 
                [ File "parser.hs" 1024 "text/x-haskell"
                , File "renderer.hs" 896 "text/x-haskell"
                ]
            ]
        ]
  
  -- Создадим линзы для работы с файловой системой
  let nameL = lens getName (\fs n -> setName fs n)
        where
          getName (File n _ _) = n
          getName (Directory n _) = n
          setName (File _ s m) n = File n s m
          setName (Directory _ c) n = Directory n c
  
  let sizeL = lens getSize (\fs s -> setSize fs s)
        where
          getSize (File _ s _) = s
          getSize (Directory _ _) = 0  -- Директории сами по себе не имеют размера
          setSize (File n _ m) s = File n s m
          setSize d@(Directory _ _) _ = d  -- Нельзя установить размер директории
  
  let mimeTypeL = lens getMimeType (\fs m -> setMimeType fs m)
        where
          getMimeType (File _ _ m) = m
          getMimeType (Directory _ _) = "directory"
          setMimeType (File n s _) m = File n s m
          setMimeType d@(Directory _ _) _ = d  -- Нельзя установить MIME-тип директории
  
  let contentsL = lens getContents (\fs c -> setContents fs c)
        where
          getContents (File _ _ _) = []
          getContents (Directory _ c) = c
          setContents (File n s m) _ = File n s m  -- Нельзя установить содержимое файла
          setContents (Directory n _) c = Directory n c
  
  -- Вспомогательные функции для работы с деревом
  let isFile (File _ _ _) = True
      isFile _ = False
  
  let isDirectory (Directory _ _) = True
      isDirectory _ = False
  
  let isHaskellFile fs = isFile fs && (fs ^. mimeTypeL == "text/x-haskell")
  
  -- Получение информации о файловой системе
  putStrLn $ "Корневая директория: " ++ (fileSystem ^. nameL)
  putStrLn $ "Количество элементов в корне: " ++ show (length $ fileSystem ^. contentsL)
  
  -- Поиск всех Haskell-файлов
  let allFiles = fileSystem ^.. cosmos . filtered isFile
  let haskellFiles = filter isHaskellFile allFiles
  
  putStrLn $ "\nВсе Haskell-файлы:"
  mapM_ (\f -> putStrLn $ "- " ++ (f ^. nameL) ++ " (" ++ show (f ^. sizeL) ++ " байт)") haskellFiles
  
  -- Вычисление общего размера всех файлов
  let totalSize = sum $ map (^. sizeL) allFiles
  putStrLn $ "\nОбщий размер всех файлов: " ++ show totalSize ++ " байт"
  
  -- Обновление MIME-типа всех Haskell-файлов
  let updatedFileSystem = over (cosmos . filtered isHaskellFile) 
                               (mimeTypeL .~ "text/x-haskell-new") 
                               fileSystem
  
  let updatedHaskellFiles = updatedFileSystem ^.. cosmos . filtered isFile . 
                                                filtered (\f -> f ^. mimeTypeL == "text/x-haskell-new")
  
  putStrLn $ "\nОбновленные Haskell-файлы:"
  mapM_ (\f -> putStrLn $ "- " ++ (f ^. nameL) ++ " (" ++ (f ^. mimeTypeL) ++ ")") updatedHaskellFiles
  
  -- Добавление нового файла в директорию src
  let addFileToSrc = over (cosmos . filtered (\d -> isDirectory d && d ^. nameL == "src") . contentsL) 
                          (++ [File "app.hs" 1280 "text/x-haskell"]) 
  
  let fileSystemWithNewFile = addFileToSrc fileSystem
  let srcFiles = fileSystemWithNewFile ^.. cosmos . filtered (\d -> isDirectory d && d ^. nameL == "src") . 
                                         contentsL . traverse . filtered isFile
  
  putStrLn $ "\nФайлы в директории src после добавления нового файла:"
  mapM_ (\f -> putStrLn $ "- " ++ (f ^. nameL)) srcFiles

-- Структуры данных для примера 3
data FileSystem = File String Int String  -- имя, размер, MIME-тип
                | Directory String [FileSystem]  -- имя, содержимое
                deriving (Show, Eq)

-- Пример 4: Работа с JSON-подобными данными
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Работа с JSON-подобными данными"
  
  -- Определим структуру для JSON-подобных данных
  let jsonData = Object $ Map.fromList
        [ ("name", String "John Doe")
        , ("age", Number 30)
        , ("isActive", Boolean True)
        , ("address", Object $ Map.fromList
            [ ("street", String "123 Main St")
            , ("city", String "New York")
            , ("zipCode", String "10001")
            ]
          )
        , ("tags", Array
            [ String "developer"
            , String "haskell"
            , String "functional"
            ]
          )
        , ("projects", Array
            [ Object $ Map.fromList
                [ ("name", String "Project A")
                , ("status", String "completed")
                ]
            , Object $ Map.fromList
                [ ("name", String "Project B")
                , ("status", String "in-progress")
                ]
            ]
          )
        ]
  
  -- Создадим линзы для работы с JSON-подобными данными
  let _String (String s) = Just s
      _String _ = Nothing
  
  let _Number (Number n) = Just n
      _Number _ = Nothing
  
  let _Boolean (Boolean b) = Just b
      _Boolean _ = Nothing
  
  let _Array (Array a) = Just a
      _Array _ = Nothing
  
  let _Object (Object o) = Just o
      _Object _ = Nothing
  
  -- Создадим призмы для работы с JSON-подобными данными
  let _StringP = prism String _String
  let _NumberP = prism Number _Number
  let _BooleanP = prism Boolean _Boolean
  let _ArrayP = prism Array _Array
  let _ObjectP = prism Object _Object
  
  -- Создадим линзы для доступа к полям объекта
  let key k = lens (\obj -> Map.findWithDefault Null k (getObject obj)) 
                   (\obj v -> setObject obj k v)
        where
          getObject (Object o) = o
          getObject _ = Map.empty
          setObject (Object o) k v = Object (Map.insert k v o)
          setObject other _ _ = other
  
  -- Создадим линзы для доступа к элементам массива
  let ix' i = lens (\arr -> getArrayItem i arr) 
                   (\arr v -> setArrayItem i v arr)
        where
          getArrayItem i (Array a) = if i >= 0 && i < length a then a !! i else Null
          getArrayItem _ _ = Null
          setArrayItem i v (Array a) = 
            if i >= 0 && i < length a 
            then Array (take i a ++ [v] ++ drop (i+1) a)
            else Array a
          setArrayItem _ _ other = other
  
  -- Получение данных из JSON
  putStrLn "Данные из JSON:"
  putStrLn $ "Имя: " ++ fromMaybe "" (jsonData ^? key "name" . _StringP)
  putStrLn $ "Возраст: " ++ show (fromMaybe 0 (jsonData ^? key "age" . _NumberP))
  putStrLn $ "Активен: " ++ show (fromMaybe False (jsonData ^? key "isActive" . _BooleanP))
  putStrLn $ "Город: " ++ fromMaybe "" (jsonData ^? key "address" . key "city" . _StringP)
  
  -- Получение списка тегов
  let tags = jsonData ^.. key "tags" . _ArrayP . traverse . _StringP
  putStrLn $ "\nТеги: " ++ intercalate ", " tags
  
  -- Получение списка проектов
  let projectNames = jsonData ^.. key "projects" . _ArrayP . traverse . key "name" . _StringP
  putStrLn $ "\nПроекты: " ++ intercalate ", " projectNames
  
  -- Обновление данных в JSON
  let updatedJsonData = jsonData & key "age" .~ Number 31
                                & key "address" . key "city" .~ String "Boston"
                                & key "tags" . _ArrayP %~ (++ [String "expert"])
  
  putStrLn $ "\nПосле обновления:"
  putStrLn $ "Возраст: " ++ show (fromMaybe 0 (updatedJsonData ^? key "age" . _NumberP))
  putStrLn $ "Город: " ++ fromMaybe "" (updatedJsonData ^? key "address" . key "city" . _StringP)
  
  let updatedTags = updatedJsonData ^.. key "tags" . _ArrayP . traverse . _StringP
  putStrLn $ "Теги: " ++ intercalate ", " updatedTags
  
  -- Добавление нового проекта
  let newProject = Object $ Map.fromList
        [ ("name", String "Project C")
        , ("status", String "planning")
        ]
  
  let jsonDataWithNewProject = updatedJsonData & key "projects" . _ArrayP %~ (++ [newProject])
  
  let allProjects = jsonDataWithNewProject ^.. key "projects" . _ArrayP . traverse
  putStrLn $ "\nВсе проекты после добавления нового:"
  mapM_ (\p -> putStrLn $ "- " ++ 
                fromMaybe "" (p ^? key "name" . _StringP) ++ 
                " (" ++ fromMaybe "" (p ^? key "status" . _StringP) ++ ")") 
        allProjects

-- Структуры данных для примера 4
data JsonValue = Null
               | String String
               | Number Int
               | Boolean Bool
               | Array [JsonValue]
               | Object (Map.Map String JsonValue)
               deriving (Show, Eq)

-- Главная функция
main :: IO ()
main = do
  putStrLn "Практическое применение линз в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о практическом применении линз:"
  putStrLn "1. Линзы упрощают работу с конфигурационными файлами и вложенными структурами данных"
  putStrLn "2. Линзы позволяют легко фильтровать, обновлять и трансформировать данные"
  putStrLn "3. Линзы особенно полезны при работе с деревьями данных и рекурсивными структурами"
  putStrLn "4. Линзы делают код более декларативным и выразительным"
  putStrLn "5. Линзы позволяют работать с данными в функциональном стиле, сохраняя неизменяемость"
  putStrLn "6. Линзы можно комбинировать для создания сложных трансформаций данных"
