{-# LANGUAGE OverloadedStrings #-}

module LibModule where

import Data.List (intercalate)
import qualified Types as T

type VkApiUrl = String

genVkApiUrl :: String -> String -> [(String, String)] -> String
genVkApiUrl token method params =
  concat ["https://api.vk.com/method/", method, "?", paramString]
  where
    paramString =
      intercalate "&" . map (\(k, v) -> k ++ "=" ++ v) $
      ("access_token", token) : ("v", "5.139") : params

vkLongpoll :: String -> String -> String -> String -> String
vkLongpoll server key timeout ts =
  concat [server, "?act=a_check&key=", key, "&ts=", ts, "&wait=", timeout]

type TgApiUrl = String

type VkApiMethod = String

type VkApiParam = (String, String)

type VkUrlGen = (VkApiMethod -> [VkApiParam] -> VkApiUrl)

data TgApiUrlGen =
  TgApiUrlGen
    { getUpdates :: Int -> TgApiUrl
    , sendMessage :: Int -> String -> TgApiUrl
    }

mkTgApiUrlGen :: T.TelegramConfig -> TgApiUrlGen
mkTgApiUrlGen config =
  TgApiUrlGen {getUpdates = getUpdatesFn, sendMessage = sendMessageFn}
  where
    tgBaseUrl = "https://api.telegram.org/bot" ++ T.telegramToken config
    getUpdatesFn offset =
      concat
        [ tgBaseUrl
        , "/getUpdates?offset="
        , show offset
        , "&timeout=" ++ show (T.telegramTimeout config)
        ]
    sendMessageFn chatid messageText =
      concat
        [tgBaseUrl, "/sendMessage?chat_id=", show chatid, "&text=", messageText]


type LogFn m = String -> m ()

data Logger m =
  Logger
    { logError :: LogFn m
    , logInfo :: LogFn m
    , logDebug :: LogFn m
    }

mkLogger :: Monad m => (T.LogLevel -> String -> m ()) -> T.LogLevel -> Logger m
mkLogger printFn maxLevel =
  Logger
    { logError = getLogFn printFn T.ErrorLogLevel
    , logInfo = getLogFn printFn T.InfoLogLevel
    , logDebug = getLogFn printFn T.DebugLogLevel
    }
  where
    getLogFn fn level =
      if level <= maxLevel
        then fn level . (++) ("[" ++ levelStr level ++ "] ")
        else const $ return ()
    levelStr T.ErrorLogLevel = "ERROR"
    levelStr T.InfoLogLevel = "INFO"
    levelStr T.DebugLogLevel = "DEBUG"
