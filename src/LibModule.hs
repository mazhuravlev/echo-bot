{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LibModule where

import Data.Aeson.Types
import Data.List (intercalate, stripPrefix)
import qualified Data.Text as Text
import Data.Text.Manipulate (toCamel, toSnake)
import Dhall (FromDhall, Generic, Natural)

type VkApiUrl = String

genVkApiUrl :: String -> [Char] -> [(String, String)] -> String
genVkApiUrl token method params =
  concat ["https://api.vk.com/method/", method, "?", paramString]
  where
    paramString =
      intercalate "&" . map (\(k, v) -> k ++ "=" ++ v) $
      ("access_token", token) : ("v", "5.139") : params

vkLongpoll :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
vkLongpoll server key timeout ts =
  concat [server, "?act=a_check&key=", key, "&ts=", ts, "&wait=", timeout]

type TgApiUrl = String

data TgApiUrlGen =
  TgApiUrlGen
    { getUpdates :: Int -> TgApiUrl
    , sendMessage :: Int -> String -> TgApiUrl
    }

mkTgApiUrlGen :: TelegramConfig -> TgApiUrlGen
mkTgApiUrlGen config =
  TgApiUrlGen {getUpdates = getUpdatesFn, sendMessage = sendMessageFn}
  where
    tgBaseUrl = "https://api.telegram.org/bot" ++ telegramToken config
    getUpdatesFn offset =
      concat
        [ tgBaseUrl
        , "/getUpdates?offset="
        , show offset
        , "&timeout=" ++ show (telegramTimeout config)
        ]
    sendMessageFn chatid messageText =
      concat
        [tgBaseUrl, "/sendMessage?chat_id=", show chatid, "&text=", messageText]

data Chat =
  Chat
    { chatId :: Int
    , chatUsername :: String
    }
  deriving (Show, Generic, ToJSON)

instance FromJSON Chat where
  parseJSON =
    genericParseJSON
      defaultOptions {fieldLabelModifier = jsonFieldToCamelWithoutPrefix "chat"}

data Message =
  Message
    { message_id :: Int
    , text :: String
    , chat :: Chat
    }
  deriving (Show, Generic, ToJSON, FromJSON)

data Update =
  Update
    { update_id :: Int
    , message :: Message
    }
  deriving (Show, Generic, ToJSON, FromJSON)

data Updates =
  Updates
    { updatesOk :: Bool
    , updatesResult :: [Update]
    }
  deriving (Show, Generic, ToJSON)

instance FromJSON Updates where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = jsonFieldToCamelWithoutPrefix "updates"}

newtype SendMessageResult =
  SendMessageResult
    { sendMessageOk :: Bool
    }
  deriving (Show, Generic)

instance FromJSON SendMessageResult where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = jsonFieldToCamelWithoutPrefix "sendMessage"}

data TelegramConfig =
  TelegramConfig
    { telegramToken :: String
    , telegramTimeout :: Natural
    }
  deriving (Generic, Show, FromDhall)

data VkConfig =
  VkConfig
    { vkToken :: String
    , vkGroupId :: String
    , vkTimeout :: Natural
    }
  deriving (Generic, Show, FromDhall)

data BotConfig =
  BotConfig
    { telegramConfig :: TelegramConfig
    , vkConfig :: VkConfig
    , botLogLevel :: LogLevel
    , runVkBot :: Bool
    }
  deriving (Generic, Show, FromDhall)

data LongPollServerResponse =
  LongPollServerResponse
    { longPollServerResponseKey :: String
    , longPollServerResponseServer :: String
    , longPollServerResponseTs :: String
    }
  deriving (Generic, Show)

instance FromJSON LongPollServerResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier =
            jsonFieldToCamelWithoutPrefix "longPollServerResponse"
        }

newtype LongPollServer =
  LongPollServer
    { longPollServerResponse :: LongPollServerResponse
    }
  deriving (Generic, Show)

instance FromJSON LongPollServer where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = jsonFieldToCamelWithoutPrefix "longPollServer"}

data VkUpdateObject =
  VkUpdateObject
    { vkUpdateObjectId :: Int
    , vkUpdateObjectDate :: Int
    , vkUpdateObjectUserId :: Int
    , vkUpdateObjectTitle :: String
    , vkUpdateObjectBody :: String
    }
  deriving (Generic, Show)

instance FromJSON VkUpdateObject where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = jsonFieldToSnakeWithoutPrefix "vkUpdateObject"}

data VkUpdate =
  VkUpdate
    { vkUpdateType :: String
    , vkUpdateObject :: VkUpdateObject
    , vkUpdateGroupId :: Int
    }
  deriving (Generic, Show)

instance FromJSON VkUpdate where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = jsonFieldToSnakeWithoutPrefix "vkUpdate"}

data VkUpdates =
  VkUpdates
    { vkUpdatesTs :: String
    , vkUpdatesUpdates :: [VkUpdate]
    }
  deriving (Generic, Show)

instance FromJSON VkUpdates where
  parseJSON =
    genericParseJSON
      defaultOptions
        {fieldLabelModifier = jsonFieldToSnakeWithoutPrefix "vkUpdates"}

jsonFieldToSnakeWithoutPrefix :: [Char] -> [Char] -> [Char]
jsonFieldToSnakeWithoutPrefix prefix xs =
  case stripPrefix prefix xs of
    Just s -> Text.unpack . toSnake . Text.pack $ s
    -- TODO: a error in json field label, figure out how to detect it with type system
    Nothing -> error "Missing prefix: " ++ prefix

jsonFieldToCamelWithoutPrefix :: [Char] -> [Char] -> [Char]
jsonFieldToCamelWithoutPrefix prefix xs =
  case stripPrefix prefix xs of
    Just s -> Text.unpack . toCamel . Text.pack $ s
    -- TODO: a error in json field label, figure out how to detect it with type system
    Nothing -> error "Missing prefix: " ++ prefix

type LogFn m = String -> m ()

data Logger m =
  Logger
    { logError :: LogFn m
    , logInfo :: LogFn m
    , logDebug :: LogFn m
    }

data LogLevel
  = ErrorLogLevel
  | InfoLogLevel
  | DebugLogLevel
  deriving (Enum, Eq, Ord, Show, Generic)

instance FromDhall LogLevel

mkLogger :: Monad m => (LogLevel -> String -> m ()) -> LogLevel -> Logger m
mkLogger printFn maxLevel =
  Logger
    { logError = getLogFn printFn ErrorLogLevel
    , logInfo = getLogFn printFn InfoLogLevel
    , logDebug = getLogFn printFn DebugLogLevel
    }
  where
    getLogFn fn level =
      if level <= maxLevel
        then fn level . (++) ("[" ++ levelStr level ++ "] ")
        else const $ return ()
    levelStr ErrorLogLevel = "ERROR"
    levelStr InfoLogLevel = "INFO"
    levelStr DebugLogLevel = "DEBUG"
