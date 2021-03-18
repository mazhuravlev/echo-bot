{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module LibModule where

import Control.Lens (preview)
import Control.Monad.State
import Data.Aeson (eitherDecode)
import Data.Aeson.Lens (AsNumber (_Number), key, _String)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Time (UTCTime)
import qualified Types as T

type VkApiUrl = String

type VkApiMethod = String

type VkApiParam = (String, String)

type VkUrlGen = (VkApiMethod -> [VkApiParam] -> VkApiUrl)

genVkApiUrl :: String -> String -> [(String, String)] -> String
genVkApiUrl token method params =
  concat ["https://api.vk.com/method/", method, "?", paramString]
  where
    paramString =
      intercalate "&" . map (\(k, v) -> k ++ "=" ++ v) $
        ("access_token", token) : ("v", "5.139") : params

vkLongpoll :: String -> String -> String -> String -> String
vkLongpoll server skey timeout ts =
  concat [server, "?act=a_check&key=", skey, "&ts=", ts, "&wait=", timeout]

type TgApiUrl = String

type TgChatId = Int

data TgApi m = TgApi
  { tgGetUpdates :: Int -> m (Either String [T.Update]),
    tgSendMessage :: TgChatId -> String -> m (Either String String)
  }

mkTgApi :: Monad m => T.TelegramConfig -> (String -> m BS.ByteString) -> TgApi m
mkTgApi config httpReq = TgApi {tgGetUpdates = getUpdatesFn, tgSendMessage = sendMessageFn}
  where
    getUpdatesFn offset = do
      updatesJson <- httpReq $ getUpdatesUrl urlGen offset
      return $ case eitherDecode (LBS.fromStrict updatesJson) :: Either String T.Updates of
        Left err -> jsonDecodeError err updatesJson
        Right updates@(T.updatesOk -> True) -> Right . T.updatesResult $ updates
        _ -> Left "Unexpected error occured on receveing updates"
    sendMessageFn chatId msg = do
      messageResultjson <- httpReq $ sendMessageUrl urlGen chatId msg
      return $ case eitherDecode (LBS.fromStrict messageResultjson) :: Either String T.SendMessageResult of
        Left err -> jsonDecodeError err messageResultjson
        Right (T.sendMessageOk -> True) -> Right $ "Message sent to chat " ++ show chatId
        _ -> Left $ "Failed to send message to chat " ++ show chatId
    urlGen = mkTgApiUrlGen config
    jsonDecodeError err json = Left $ "Response JSON decode error: " ++ err ++ show json

tgGetChats :: [T.Update] -> [(Int, String)]
tgGetChats = map ((\x -> (T.chatId . T.chat $ x, T.text x)) . T.message)

tgNextOffset :: Int -> [T.Update] -> Int
tgNextOffset offset [] = offset
tgNextOffset _ updates = (+ 1) . foldr (max . T.update_id) 0 $ updates

data TgApiUrlGen = TgApiUrlGen
  { getUpdatesUrl :: Int -> TgApiUrl,
    sendMessageUrl :: TgChatId -> String -> TgApiUrl
  }

mkTgApiUrlGen :: T.TelegramConfig -> TgApiUrlGen
mkTgApiUrlGen config =
  TgApiUrlGen {getUpdatesUrl = getUpdatesFn, sendMessageUrl = sendMessageFn}
  where
    tgBaseUrl = "https://api.telegram.org/bot" ++ T.telegramToken config
    getUpdatesFn offset =
      concat
        [ tgBaseUrl,
          "/getUpdates?offset=",
          show offset,
          "&timeout=" ++ show (T.telegramTimeout config)
        ]
    sendMessageFn chatid messageText =
      concat
        [tgBaseUrl, "/sendMessage?chat_id=", show chatid, "&text=", messageText]

type LogFn m = String -> m ()

data Logger m = Logger
  { logError :: LogFn m,
    logInfo :: LogFn m,
    logDebug :: LogFn m
  }

mkLogger :: Monad m => (T.LogLevel -> String -> m ()) -> T.LogLevel -> m UTCTime -> Logger m
mkLogger printFn maxLevel getTime =
  Logger
    { logError = getLogFn printFn T.ErrorLogLevel,
      logInfo = getLogFn printFn T.InfoLogLevel,
      logDebug = getLogFn printFn T.DebugLogLevel
    }
  where
    getLogFn fn level =
      if level <= maxLevel
        then (\msg -> do time <- getTime; fn level ("[" ++ levelStr level ++ "] " ++ take 19 (show time) ++ " " ++ msg))
        else const $ return ()
    levelStr T.ErrorLogLevel = "ERROR"
    levelStr T.InfoLogLevel = "INFO"
    levelStr T.DebugLogLevel = "DEBUG"

parseVkMessageSendResult :: BS.ByteString -> Either String Int
parseVkMessageSendResult json = maybe err Right okMaybe
  where
    okMaybe = round <$> preview (key "response" . _Number) json :: Maybe Int
    errorMsg = preview (key "error" . key "error_msg" . _String) json
    errorCode = round <$> preview (key "error" . key "error_code" . _Number) json :: Maybe Int
    errMaybe = (\m c -> unpack m ++ "; error code: " ++ show c) <$> errorMsg <*> errorCode
    err = maybe (Left $ "Unexpected error on json: " ++ show json) Left errMaybe

tgProcessMessage :: Monad m => Logger m -> TgApi m -> UserMap -> (Int, String) -> m UserMap
tgProcessMessage logger tgApi userMap (chatid, messageText) = do
  (op, msg) <- case messageText of
    "/help" -> return (ReplyOnce, "this is help message!")
    ('/' : 'r' : 'e' : 'p' : 'e' : 'a' : 't' : ' ' : (parseNumber -> Just n)) -> return (Repeat n, "New repeat setting stored: " ++ show n)
    _ -> return (Reply, messageText)
  let storedRptCnt = fromMaybe 3 (Map.lookup chatid userMap)
  let newRepCnt = newRepeatCount op storedRptCnt
  replicateM_ (repeatCount op newRepCnt) (doSendMsg msg)
  return $ Map.insert chatid newRepCnt userMap
  where
    doSendMsg msg' = do
      sendResult <- tgSendMessage tgApi chatid msg'
      case sendResult of
        Left err -> logError logger $ "Telegram.sendMessage FAIL decoding json response: " ++ err
        Right _ -> logInfo logger $ "Telegram.sendMessage OK chat_id " ++ show chatid

type UserMap = Map.Map Int Int

data CommpandOp = Repeat Int | ReplyOnce | Reply

newRepeatCount :: CommpandOp -> Int -> Int
newRepeatCount (Repeat n) _ = n
newRepeatCount _ x = x

repeatCount :: CommpandOp -> Int -> Int
repeatCount Reply n = n
repeatCount _ _ = 1

parseNumber :: [Char] -> Maybe Int
parseNumber x = if null digits then Nothing else Just . read $ digits
  where
    digits = filter isDigit x

type TgState m = (Logger m, TgApi m, UserMap)

tgState :: Monad m => StateT (TgState m) m (TgState m)
tgState = do get

tgLogger :: Monad m => StateT (TgState m) m (Logger m)
tgLogger = do
  (l, _, _) <- tgState
  return l

tgLoop :: Monad m => Int -> StateT (TgState m) m ()
tgLoop offset = do
  (_, api, userMap) <- get
  logger <- tgLogger
  eitherUpdates <- lift (tgGetUpdates api offset)
  case eitherUpdates of
    Right updates -> do
      lift ((if null updates then logDebug else logInfo) logger $ formatUpdatesLog updates)
      userMap' <- lift (foldM (tgProcessMessage logger api) userMap $ tgGetChats updates)
      put (logger, api, userMap')
      tgLoop (tgNextOffset offset updates)
    Left err -> do 
      lift (logError logger $ "Failed to get updates: " ++ err)
      tgLoop offset
  return ()
  where
    formatUpdatesLog updates' =
      "Received "
        ++ (show . length $ updates')
        ++ " Telegram updates"