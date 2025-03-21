{-
  Работа с сетью в Haskell
  
  В этом файле мы рассмотрим операции ввода-вывода для работы с сетью,
  включая создание клиентов и серверов, HTTP-запросы и другие сетевые операции.
-}

module Main where

import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.Socket.ByteString.Lazy as LazySocket
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void, when, forM_)
import Control.Exception (bracket, try, SomeException, finally)
import System.IO
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Network.HTTP.Simple
import Network.HTTP.Client (HttpException, RequestBody(RequestBodyBS))
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (HeaderName, hUserAgent)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson ((.:), (.=), Key, FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- Работа с сетью в Haskell осуществляется через модули
-- Network.Socket, Network.HTTP.Simple и другие.

-- Пример 1: Базовые операции с сокетами

-- Создание TCP-сокета
createTCPSocket :: IO Socket
createTCPSocket = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  return sock

-- Привязка сокета к адресу и порту
bindSocket :: Socket -> HostName -> ServiceName -> IO ()
bindSocket sock host port = do
  addrInfo <- getAddrInfo 
                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                (Just host) 
                (Just port)
  let serverAddr = head addrInfo
  bind sock (addrAddress serverAddr)

-- Прослушивание входящих соединений
listenSocket :: Socket -> Int -> IO ()
listenSocket sock backlog = listen sock backlog

-- Принятие входящего соединения
acceptConnection :: Socket -> IO (Socket, SockAddr)
acceptConnection sock = accept sock

-- Подключение к серверу
connectToServer :: HostName -> ServiceName -> IO Socket
connectToServer host port = do
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  return sock

-- Отправка данных через сокет
sendData :: Socket -> BS.ByteString -> IO ()
sendData sock msg = sendAll sock msg

-- Получение данных из сокета
receiveData :: Socket -> Int -> IO BS.ByteString
receiveData sock maxBytes = recv sock maxBytes

-- Закрытие сокета
closeSocket :: Socket -> IO ()
closeSocket = close

-- Пример 2: Простой эхо-сервер

-- Обработка клиентского соединения
handleClient :: Socket -> IO ()
handleClient clientSock = do
  msg <- receiveData clientSock 1024
  when (not $ BS.null msg) $ do
    sendData clientSock msg
    handleClient clientSock

-- Запуск эхо-сервера
runEchoServer :: HostName -> ServiceName -> IO ()
runEchoServer host port = do
  bracket createTCPSocket closeSocket $ \sock -> do
    bindSocket sock host port
    listenSocket sock 5
    putStrLn $ "Эхо-сервер запущен на " ++ host ++ ":" ++ port
    forever $ do
      (clientSock, clientAddr) <- acceptConnection sock
      putStrLn $ "Принято соединение от " ++ show clientAddr
      void $ forkIO $ finally 
        (handleClient clientSock) 
        (closeSocket clientSock)

-- Пример 3: Простой эхо-клиент

-- Запуск эхо-клиента
runEchoClient :: HostName -> ServiceName -> IO ()
runEchoClient host port = do
  bracket (connectToServer host port) closeSocket $ \sock -> do
    putStrLn $ "Подключено к серверу " ++ host ++ ":" ++ port
    let message = C8.pack "Привет, сервер!"
    putStrLn $ "Отправка: " ++ C8.unpack message
    sendData sock message
    response <- receiveData sock 1024
    putStrLn $ "Получено: " ++ C8.unpack response

-- Пример 4: HTTP-запросы

-- Выполнение GET-запроса
httpGet :: String -> IO BL.ByteString
httpGet url = do
  request <- parseRequest url
  response <- httpLBS request
  return $ getResponseBody response

-- Выполнение POST-запроса
httpPost :: String -> BS.ByteString -> IO BL.ByteString
httpPost url body = do
  initialRequest <- parseRequest url
  let request = setRequestMethod (C8.pack "POST")
              $ setRequestBody (RequestBodyBS body)
              $ initialRequest
  response <- httpLBS request
  return $ getResponseBody response

-- Выполнение запроса с заголовками
httpWithHeaders :: String -> [(String, BS.ByteString)] -> IO BL.ByteString
httpWithHeaders url headers = do
  initialRequest <- parseRequest url
  let request = foldr (\(name, value) req -> setRequestHeader hUserAgent [value] req) 
                      initialRequest 
                      headers
  response <- httpLBS request
  return $ getResponseBody response

-- Пример 5: Работа с JSON через API

-- Определение типа данных для пользователя
data User = User
  { userId :: Int
  , userName :: String
  , userEmail :: String
  } deriving (Show, Eq)

-- Получение данных пользователя по ID
getUserData :: Int -> IO String
getUserData id = do
  let url = "https://jsonplaceholder.typicode.com/users/" ++ show id
  result <- try (httpGet url) :: IO (Either HttpException BL.ByteString)
  case result of
    Left err -> do
      putStrLn $ "Ошибка при выполнении запроса: " ++ show err
      return "Ошибка при получении данных пользователя"
    Right response -> do
      return $ "Получены данные пользователя: " ++ take 100 (show response) ++ "..."

-- Получение списка пользователей
getUsersData :: IO String
getUsersData = do
  let url = "https://jsonplaceholder.typicode.com/users"
  result <- try (httpGet url) :: IO (Either HttpException BL.ByteString)
  case result of
    Left err -> do
      putStrLn $ "Ошибка при выполнении запроса: " ++ show err
      return "Ошибка при получении списка пользователей"
    Right response -> do
      return $ "Получен список пользователей: " ++ take 100 (show response) ++ "..."

-- Создание нового пользователя
createUserData :: User -> IO String
createUserData user = do
  let url = "https://jsonplaceholder.typicode.com/users"
      userJson = "{\"id\":" ++ show (userId user) ++ 
                 ",\"name\":\"" ++ userName user ++ 
                 "\",\"email\":\"" ++ userEmail user ++ "\"}"
      body = C8.pack userJson
  result <- try (httpPost url body) :: IO (Either HttpException BL.ByteString)
  case result of
    Left err -> do
      putStrLn $ "Ошибка при выполнении запроса: " ++ show err
      return "Ошибка при создании пользователя"
    Right response -> do
      return $ "Создан пользователь: " ++ take 100 (show response) ++ "..."

-- Пример 6: Простой HTTP-сервер

-- Обработка HTTP-запроса
handleHTTPRequest :: Socket -> IO ()
handleHTTPRequest clientSock = do
  request <- receiveData clientSock 4096
  let requestLines = C8.lines request
      firstLine = if not (null requestLines) then head requestLines else C8.empty
      method = C8.unpack $ head $ C8.words firstLine
      path = C8.unpack $ if length (C8.words firstLine) > 1 
                         then (C8.words firstLine) !! 1 
                         else C8.empty
  
  putStrLn $ "Получен запрос: " ++ method ++ " " ++ path
  
  let response = case (method, path) of
        ("GET", "/") -> 
          "HTTP/1.1 200 OK\r\n" ++
          "Content-Type: text/html; charset=utf-8\r\n" ++
          "\r\n" ++
          "<html><body><h1>Привет от Haskell HTTP-сервера!</h1></body></html>"
        
        ("GET", "/api/users") ->
          "HTTP/1.1 200 OK\r\n" ++
          "Content-Type: application/json; charset=utf-8\r\n" ++
          "\r\n" ++
          "[{\"id\":1,\"name\":\"Иван\",\"email\":\"ivan@example.com\"}," ++
          "{\"id\":2,\"name\":\"Мария\",\"email\":\"maria@example.com\"}]"
        
        _ ->
          "HTTP/1.1 404 Not Found\r\n" ++
          "Content-Type: text/plain; charset=utf-8\r\n" ++
          "\r\n" ++
          "404 Not Found"
  
  sendData clientSock (C8.pack response)

-- Запуск HTTP-сервера
runHTTPServer :: HostName -> ServiceName -> IO ()
runHTTPServer host port = do
  bracket createTCPSocket closeSocket $ \sock -> do
    bindSocket sock host port
    listenSocket sock 5
    putStrLn $ "HTTP-сервер запущен на " ++ host ++ ":" ++ port
    forever $ do
      (clientSock, clientAddr) <- acceptConnection sock
      putStrLn $ "Принято соединение от " ++ show clientAddr
      void $ forkIO $ finally 
        (handleHTTPRequest clientSock) 
        (closeSocket clientSock)

-- Пример 7: Асинхронный сетевой ввод-вывод

-- Обработка нескольких клиентов одновременно
handleMultipleClients :: [(Socket, SockAddr)] -> IO ()
handleMultipleClients clients = do
  forM_ clients $ \(clientSock, clientAddr) -> do
    void $ forkIO $ do
      putStrLn $ "Обработка клиента " ++ show clientAddr
      handleClient clientSock
      putStrLn $ "Клиент " ++ show clientAddr ++ " обработан"

-- Пример 1: Базовые операции с сокетами
example1 :: IO ()
example1 = do
  putStrLn "Пример 1: Базовые операции с сокетами"
  
  -- Создание сокета
  putStrLn "Создание TCP-сокета..."
  sock <- createTCPSocket
  
  -- Вывод информации о сокете
  sockAddr <- getSocketName sock
  putStrLn $ "Сокет создан: " ++ show sockAddr
  
  -- Закрытие сокета
  putStrLn "Закрытие сокета..."
  closeSocket sock
  
  putStrLn "Базовые операции с сокетами выполнены."

-- Пример 2: Простой эхо-сервер (не запускаем, только демонстрация)
example2 :: IO ()
example2 = do
  putStrLn "\nПример 2: Простой эхо-сервер"
  
  putStrLn "Для запуска эхо-сервера используйте:"
  putStrLn "runEchoServer \"127.0.0.1\" \"8080\""
  
  putStrLn "Сервер будет принимать соединения и отправлять обратно полученные данные."
  putStrLn "Для тестирования можно использовать telnet или netcat:"
  putStrLn "telnet 127.0.0.1 8080"
  putStrLn "или"
  putStrLn "nc 127.0.0.1 8080"

-- Пример 3: Простой эхо-клиент (не запускаем, только демонстрация)
example3 :: IO ()
example3 = do
  putStrLn "\nПример 3: Простой эхо-клиент"
  
  putStrLn "Для запуска эхо-клиента используйте:"
  putStrLn "runEchoClient \"127.0.0.1\" \"8080\""
  
  putStrLn "Клиент подключится к серверу, отправит сообщение и выведет ответ."

-- Пример 4: HTTP-запросы
example4 :: IO ()
example4 = do
  putStrLn "\nПример 4: HTTP-запросы"
  
  -- Выполнение GET-запроса
  putStrLn "Выполнение GET-запроса к httpbin.org..."
  result <- try (httpGet "http://httpbin.org/get") :: IO (Either HttpException BL.ByteString)
  case result of
    Left err -> putStrLn $ "Ошибка при выполнении запроса: " ++ show err
    Right response -> do
      putStrLn "Ответ получен:"
      putStrLn $ take 200 $ show response ++ "..."
  
  -- Выполнение запроса с заголовками
  putStrLn "\nВыполнение запроса с заголовками..."
  let headers = [("User-Agent", C8.pack "Haskell Network Example")]
  headerResult <- try (httpWithHeaders "http://httpbin.org/headers" headers) :: IO (Either HttpException BL.ByteString)
  case headerResult of
    Left err -> putStrLn $ "Ошибка при выполнении запроса: " ++ show err
    Right response -> do
      putStrLn "Ответ получен:"
      putStrLn $ take 200 $ show response ++ "..."

-- Пример 5: Работа с JSON через API
example5 :: IO ()
example5 = do
  putStrLn "\nПример 5: Работа с JSON через API"
  
  -- Получение пользователя по ID
  putStrLn "Получение пользователя по ID..."
  userData <- getUserData 1
  putStrLn userData
  
  -- Получение списка пользователей
  putStrLn "\nПолучение списка пользователей..."
  usersData <- getUsersData
  putStrLn usersData
  
  -- Создание нового пользователя
  putStrLn "\nСоздание нового пользователя..."
  let newUser = User 0 "Иван Иванов" "ivan@example.com"
  createdUserData <- createUserData newUser
  putStrLn createdUserData

-- Пример 6: Простой HTTP-сервер (не запускаем, только демонстрация)
example6 :: IO ()
example6 = do
  putStrLn "\nПример 6: Простой HTTP-сервер"
  
  putStrLn "Для запуска HTTP-сервера используйте:"
  putStrLn "runHTTPServer \"127.0.0.1\" \"8080\""
  
  putStrLn "Сервер будет отвечать на следующие запросы:"
  putStrLn "GET / - HTML-страница с приветствием"
  putStrLn "GET /api/users - JSON-список пользователей"
  
  putStrLn "Для тестирования можно использовать браузер или curl:"
  putStrLn "curl http://127.0.0.1:8080/"
  putStrLn "curl http://127.0.0.1:8080/api/users"

-- Пример 7: Асинхронный сетевой ввод-вывод (не запускаем, только демонстрация)
example7 :: IO ()
example7 = do
  putStrLn "\nПример 7: Асинхронный сетевой ввод-вывод"
  
  putStrLn "Асинхронный ввод-вывод позволяет обрабатывать несколько клиентов одновременно."
  putStrLn "В Haskell это реализуется с помощью функции forkIO, которая создает новый поток."
  
  putStrLn "Пример использования:"
  putStrLn "bracket createTCPSocket closeSocket $ \\sock -> do"
  putStrLn "  bindSocket sock \"127.0.0.1\" \"8080\""
  putStrLn "  listenSocket sock 5"
  putStrLn "  forever $ do"
  putStrLn "    (clientSock, clientAddr) <- acceptConnection sock"
  putStrLn "    forkIO $ handleClient clientSock"

-- Главная функция
main :: IO ()
main = do
  putStrLn "Работа с сетью в Haskell\n"
  
  -- Запуск примеров
  example1
  example2
  example3
  example4
  example5
  example6
  example7
  
  putStrLn "\nКлючевые моменты о работе с сетью в Haskell:"
  putStrLn "1. Модуль Network.Socket предоставляет низкоуровневый доступ к сокетам"
  putStrLn "2. Модуль Network.HTTP.Simple предоставляет высокоуровневый доступ к HTTP"
  putStrLn "3. Модуль Data.Aeson предоставляет функции для работы с JSON"
  putStrLn "4. Асинхронный ввод-вывод реализуется с помощью функции forkIO"
  putStrLn "5. Для обработки ошибок используются функции из модуля Control.Exception"
  putStrLn "6. Для работы с бинарными данными используются модули Data.ByteString и Data.ByteString.Lazy"
  putStrLn "7. Для работы с текстом используются модули Data.Text и Data.Text.Encoding"
