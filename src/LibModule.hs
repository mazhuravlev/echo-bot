{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LibModule where

import Data.Aeson
import Data.List (stripPrefix)
import qualified Data.Text as Text
import Data.Text.Manipulate (toCamel)
import Dhall

type TgApiUrl = String

data TgApiUrlGen =
  TgApiUrlGen
    { getUpdates :: Int -> TgApiUrl
    , sendMessage :: Int -> String -> TgApiUrl
    }

mkTgApiUrlGen :: BotConfig -> TgApiUrlGen
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

data BotConfig =
  BotConfig
    { telegramToken :: String
    , telegramTimeout :: Natural
    , botLogLevel :: LogLevel
    }
  deriving (Generic, Show)

instance FromDhall BotConfig

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

mkLogger :: Monad m => LogFn m -> LogFn m -> LogFn m -> LogLevel -> Logger m
mkLogger errorFn infoFn debugFn logLevel =
  Logger
    { logError = getLogFn errorFn ErrorLogLevel
    , logInfo = getLogFn infoFn InfoLogLevel
    , logDebug = getLogFn debugFn DebugLogLevel
    }
  where
    getLogFn fn level =
      if level <= logLevel
        then fn . (++) ("[" ++ levelStr level ++ "] ")
        else const $ return ()
    levelStr ErrorLogLevel = "ERROR"
    levelStr InfoLogLevel = "INFO"
    levelStr DebugLogLevel = "DEBUG"
