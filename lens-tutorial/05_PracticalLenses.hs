{-
  Практическое применение линз в Haskell
  
  В этом файле мы рассмотрим практические примеры использования линз
  для решения реальных задач.
-}

module Main where

import qualified Data.Map as Map
import Data.Char (toUpper, toLower)

-- Определим структуры данных для примеров
data User = User
  { _userId :: Int
  , _userName :: String
  , _userEmail :: String
  , _userSettings :: UserSettings
  , _userStats :: UserStats
  } deriving (Show, Eq)

data UserSettings = UserSettings
  { _theme :: String
  , _notifications :: Bool
  , _language :: String
  , _privacy :: PrivacySettings
  } deriving (Show, Eq)

data PrivacySettings = PrivacySettings
  { _showEmail :: Bool
  , _showActivity :: Bool
  , _allowMessages :: Bool
  } deriving (Show, Eq)

data UserStats = UserStats
  { _postsCount :: Int
  , _commentsCount :: Int
  , _likesCount :: Int
  , _lastActive :: String
  } deriving (Show, Eq)

-- Пример данных
sampleUser :: User
sampleUser = User
  { _userId = 12345
  , _userName = "ivan_ivanov"
  , _userEmail = "ivan@example.com"
  , _userSettings = UserSettings
      { _theme = "light"
      , _notifications = True
      , _language = "ru"
      , _privacy = PrivacySettings
          { _showEmail = False
          , _showActivity = True
          , _allowMessages = True
          }
      }
  , _userStats = UserStats
      { _postsCount = 42
      , _commentsCount = 156
      , _likesCount = 789
      , _lastActive = "2023-05-15"
      }
  }

-- Пример 1: Обновление настроек пользователя
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Обновление настроек пользователя"
  
  putStrLn "Исходные настройки пользователя:"
  putStrLn $ "Тема: " ++ _theme (_userSettings sampleUser)
  putStrLn $ "Уведомления: " ++ show (_notifications (_userSettings sampleUser))
  putStrLn $ "Язык: " ++ _language (_userSettings sampleUser)
  
  -- Обновление настроек пользователя
  let updatedUser = sampleUser {
        _userSettings = (_userSettings sampleUser) {
          _theme = "dark",
          _notifications = False,
          _language = "en"
        }
      }
  
  putStrLn "\nОбновленные настройки пользователя:"
  putStrLn $ "Тема: " ++ _theme (_userSettings updatedUser)
  putStrLn $ "Уведомления: " ++ show (_notifications (_userSettings updatedUser))
  putStrLn $ "Язык: " ++ _language (_userSettings updatedUser)
  
  -- Обновление настроек приватности
  let privateUser = sampleUser {
        _userSettings = (_userSettings sampleUser) {
          _privacy = PrivacySettings {
            _showEmail = False,
            _showActivity = False,
            _allowMessages = False
          }
        }
      }
  
  putStrLn "\nОбновленные настройки приватности:"
  putStrLn $ "Показывать email: " ++ show (_showEmail (_privacy (_userSettings privateUser)))
  putStrLn $ "Показывать активность: " ++ show (_showActivity (_privacy (_userSettings privateUser)))
  putStrLn $ "Разрешить сообщения: " ++ show (_allowMessages (_privacy (_userSettings privateUser)))

-- Пример 2: Работа с пользовательскими данными
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Работа с пользовательскими данными"
  
  -- Форматирование имени пользователя
  let formattedUser = sampleUser {
        _userName = map toUpper (_userName sampleUser),
        _userEmail = map toLower (_userEmail sampleUser)
      }
  
  putStrLn $ "Исходное имя пользователя: " ++ _userName sampleUser
  putStrLn $ "Отформатированное имя пользователя: " ++ _userName formattedUser
  
  putStrLn $ "Исходный email пользователя: " ++ _userEmail sampleUser
  putStrLn $ "Отформатированный email пользователя: " ++ _userEmail formattedUser
  
  -- Обновление статистики пользователя
  let activeUser = sampleUser {
        _userStats = (_userStats sampleUser) {
          _postsCount = _postsCount (_userStats sampleUser) + 1,
          _commentsCount = _commentsCount (_userStats sampleUser) + 5,
          _likesCount = _likesCount (_userStats sampleUser) + 10,
          _lastActive = "2023-05-16"
        }
      }
  
  putStrLn "\nИсходная статистика пользователя:"
  putStrLn $ "Количество постов: " ++ show (_postsCount (_userStats sampleUser))
  putStrLn $ "Количество комментариев: " ++ show (_commentsCount (_userStats sampleUser))
  putStrLn $ "Количество лайков: " ++ show (_likesCount (_userStats sampleUser))
  putStrLn $ "Последняя активность: " ++ _lastActive (_userStats sampleUser)
  
  putStrLn "\nОбновленная статистика пользователя:"
  putStrLn $ "Количество постов: " ++ show (_postsCount (_userStats activeUser))
  putStrLn $ "Количество комментариев: " ++ show (_commentsCount (_userStats activeUser))
  putStrLn $ "Количество лайков: " ++ show (_likesCount (_userStats activeUser))
  putStrLn $ "Последняя активность: " ++ _lastActive (_userStats activeUser)

-- Пример 3: Создание составных функций для бизнес-логики
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Создание составных функций для бизнес-логики"
  
  -- Создадим составную функцию для проверки, является ли пользователь активным
  let isActiveUser user = _postsCount (_userStats user) > 0
  
  -- Создадим составную функцию для получения полного имени пользователя
  let fullUserName user = "User: " ++ _userName user
  
  -- Создадим составную функцию для получения уровня приватности
  let privacyLevel user = 
        let privacy = _privacy (_userSettings user)
            boolToInt b = if b then 0 else 1
            level = boolToInt (_showEmail privacy) + 
                    boolToInt (_showActivity privacy) + 
                    boolToInt (_allowMessages privacy)
        in if level == 0 then "Открытый"
           else if level < 3 then "Умеренный"
           else "Закрытый"
  
  -- Использование составных функций
  putStrLn $ "Активный пользователь: " ++ show (isActiveUser sampleUser)
  putStrLn $ "Полное имя пользователя: " ++ fullUserName sampleUser
  putStrLn $ "Уровень приватности: " ++ privacyLevel sampleUser
  
  -- Создадим неактивного пользователя
  let inactiveUser = sampleUser {
        _userStats = (_userStats sampleUser) {
          _postsCount = 0,
          _commentsCount = 0,
          _likesCount = 0
        }
      }
  
  putStrLn $ "\nНеактивный пользователь: " ++ show (isActiveUser inactiveUser)
  
  -- Создадим закрытого пользователя
  let closedUser = sampleUser {
        _userSettings = (_userSettings sampleUser) {
          _privacy = PrivacySettings {
            _showEmail = False,
            _showActivity = False,
            _allowMessages = False
          }
        }
      }
  
  putStrLn $ "Уровень приватности закрытого пользователя: " ++ privacyLevel closedUser

-- Пример 4: Использование функций для валидации данных
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: Использование функций для валидации данных"
  
  -- Создадим функцию для валидации email
  let validateEmail email = 
        if '@' `elem` email && length email > 5
        then Right email
        else Left "Некорректный email"
  
  -- Создадим функцию для валидации имени пользователя
  let validateUserName name =
        if all (\c -> c `elem` (['a'..'z'] ++ ['0'..'9'] ++ "_")) name && length name >= 3
        then Right name
        else Left "Некорректное имя пользователя"
  
  -- Валидация данных пользователя
  let validateUser user = do
        validEmail <- validateEmail (_userEmail user)
        validName <- validateUserName (_userName user)
        return $ user {
          _userEmail = validEmail,
          _userName = validName
        }
  
  -- Проверка валидного пользователя
  putStrLn "Валидация данных пользователя:"
  case validateUser sampleUser of
    Right validUser -> putStrLn "Пользователь валиден"
    Left error -> putStrLn $ "Ошибка валидации: " ++ error
  
  -- Проверка невалидного пользователя
  let invalidUser = sampleUser { _userEmail = "invalid" }
  
  putStrLn "\nВалидация данных невалидного пользователя:"
  case validateUser invalidUser of
    Right validUser -> putStrLn "Пользователь валиден"
    Left error -> putStrLn $ "Ошибка валидации: " ++ error

-- Главная функция
main :: IO ()
main = do
  putStrLn "Практическое применение линз в Haskell\n"
  
  example1
  example2
  example3
  example4
  
  putStrLn "\nКлючевые моменты о практическом применении линз:"
  putStrLn "1. Линзы упрощают работу с вложенными структурами данных"
  putStrLn "2. Линзы позволяют создавать выразительный API для работы с данными"
  putStrLn "3. Составные линзы могут инкапсулировать бизнес-логику"
  putStrLn "4. Линзы делают код более декларативным и понятным"
  putStrLn "5. Линзы можно использовать для валидации и трансформации данных"
