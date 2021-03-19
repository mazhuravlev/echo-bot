{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module LibModule where

import Control.Monad.State (MonadState (get, put), State)
import qualified Data.ByteString.Char8 as BS
import Data.Char (isDigit)
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Text.Manipulate (toCamel, toSnake)
import Data.Time (UTCTime)
import Dhall (FromDhall, Generic, Natural)

type UserMap = Map.Map Int Natural

type LogFn m = String -> m ()

type HttpR m = (String -> m BS.ByteString)

type UserId = Int

type MessageText = String

type Message = (UserId, MessageText)

type RepeatCount = Natural

type MessageProcessor = [Message] -> State UserMap [Instruction]

data Instruction = SendMessage RepeatCount Message

type HelpMessage = String

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

mkMessageProcessor :: HelpMessage -> Natural -> [Message] -> State UserMap [Instruction]
mkMessageProcessor helpMsg defaultRepeat = processMessages
  where
    processMessages :: [Message] -> State UserMap [Instruction]
    processMessages [] = return []
    processMessages (x : xs) = do
      let (uid, txt) = x
      um <- get
      let storedRepeat = fromMaybe defaultRepeat $ Map.lookup uid um
      i <- case txt of
        "/help" -> return $ SendMessage 1 (uid, helpMsg)
        ('/' : 'r' : 'e' : 'p' : 'e' : 'a' : 't' : ' ' : (parseNumber -> Just newRepeatCount)) -> do
          put $ Map.insert uid newRepeatCount um
          return $ SendMessage 1 (uid, "New repeat setting is " ++ show newRepeatCount)
        _ -> return $ SendMessage storedRepeat x
      is <- processMessages xs
      return (i : is)
    parseNumber x = if null digits then Nothing else Just . read $ digits
      where
        digits = filter isDigit x

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
    botType :: BotType,
    defaultRepeatCount :: Natural,
    helpMessage :: String
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
