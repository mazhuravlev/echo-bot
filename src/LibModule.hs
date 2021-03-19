{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module LibModule where

import Data.Time (UTCTime)
import qualified Data.Map as Map
import Data.List (stripPrefix)
import qualified Data.Text as Text
import Data.Text.Manipulate (toCamel, toSnake)
import Dhall (FromDhall, Generic, Natural)
import qualified Data.ByteString.Char8 as BS

type UserMap = Map.Map Int Int

type LogFn m = String -> m ()

type HttpR m = (String -> m BS.ByteString)

data Logger m = Logger
  { logError :: LogFn m,
    logInfo :: LogFn m,
    logDebug :: LogFn m
  }

mkLogger :: Monad m => (LogLevel -> String -> m ()) -> LogLevel -> m UTCTime -> Logger m
mkLogger printFn maxLevel getTime =
  Logger
    { logError = getLogFn printFn ErrorLogLevel,
      logInfo = getLogFn printFn InfoLogLevel,
      logDebug = getLogFn printFn DebugLogLevel
    }
  where
    getLogFn fn level =
      if level <= maxLevel
        then (\msg -> do time <- getTime; fn level ("[" ++ levelStr level ++ "] " ++ take 19 (show time) ++ " " ++ msg))
        else const $ return ()
    levelStr ErrorLogLevel = "ERROR"
    levelStr InfoLogLevel = "INFO"
    levelStr DebugLogLevel = "DEBUG"

data TelegramConfig = TelegramConfig
  { telegramToken :: String,
    telegramTimeout :: Natural
  }
  deriving (Generic, Show, FromDhall)

data VkConfig = VkConfig
  { vkToken :: String,
    vkGroupId :: String,
    vkTimeout :: Natural
  }
  deriving (Generic, Show, FromDhall)

data BotType = Vk | Telegram deriving (Show, Generic, FromDhall)

data BotConfig = BotConfig
  { telegramConfig :: TelegramConfig,
    vkConfig :: VkConfig,
    botLogLevel :: LogLevel,
    botType :: BotType
  }
  deriving (Generic, Show, FromDhall)

data LogLevel
  = ErrorLogLevel
  | InfoLogLevel
  | DebugLogLevel
  deriving (Enum, Eq, Ord, Show, Generic)

instance FromDhall LogLevel

jsonFieldToSnakeWithoutPrefix :: String -> String -> String
jsonFieldToSnakeWithoutPrefix prefix xs =
  case stripPrefix prefix xs of
    Just s -> Text.unpack . toSnake . Text.pack $ s
    -- TODO: a error in json field label, figure out how to detect it with type system
    Nothing -> error "Missing prefix: " ++ prefix

jsonFieldToCamelWithoutPrefix :: String -> String -> String
jsonFieldToCamelWithoutPrefix prefix xs =
  case stripPrefix prefix xs of
    Just s -> Text.unpack . toCamel . Text.pack $ s
    -- TODO: a error in json field label, figure out how to detect it with type system
    Nothing -> error "Missing prefix: " ++ prefix
