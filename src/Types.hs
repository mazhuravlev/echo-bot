{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Manipulate (toCamel, toSnake)
import Dhall (FromDhall, Generic, Natural)

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

data BotConfig = BotConfig
  { telegramConfig :: TelegramConfig,
    vkConfig :: VkConfig,
    botLogLevel :: LogLevel,
    runVkBot :: Bool
  }
  deriving (Generic, Show, FromDhall)

data Chat = Chat
  { chatId :: Int,
    chatUsername :: String
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON Chat where
  parseJSON =
    genericParseJSON
      defaultOptions {fieldLabelModifier = jsonFieldToCamelWithoutPrefix "chat"}

data MessageType = EditedMessage | NewMessage deriving (Show, Eq)

data Message = Message
  { tgMessageType :: MessageType,
    message_id :: Int,
    text :: String,
    chat :: Chat
  }
  deriving (Show)

data Update = Update
  { update_id :: Int,
    message :: Message
  }
  deriving (Show)

instance FromJSON Update where
  parseJSON = withObject "Update" $ \obj -> do
    updateId <- obj .: "update_id"
    let isMessage = "message" `HM.member` obj
    let isEditedMessage = "edited_message" `HM.member` obj
    let (messageKey, msgType) = case (isMessage, isEditedMessage) of
          (True, False) -> ("message" :: Text, NewMessage)
          (False, True) -> ("edited_message", EditedMessage)
          -- TODO: use fail
          _ -> error $ "Update.parseJSON invalid json: " ++ show obj
    -- TODO: get rid of fromJust
    let messageObj = fromJust $ HM.lookup messageKey obj
    msg <-
      withObject
        "Message"
        ( \mobj -> do
            mid <- mobj .: "message_id"
            t <- mobj .: "text"
            -- TODO: get rid of fromJust
            c <- fromJust (parseJSON <$> HM.lookup "chat" mobj) :: Parser Chat
            return Message {tgMessageType = msgType, message_id = mid, text = t, chat = c}
        )
        messageObj

    return $ Update {update_id = updateId, message = msg}

data Updates = Updates
  { updatesOk :: Bool,
    updatesResult :: [Update]
  }
  deriving (Show, Generic)

instance FromJSON Updates where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldToCamelWithoutPrefix "updates"
        }

newtype SendMessageResult = SendMessageResult
  { sendMessageOk :: Bool
  }
  deriving (Show, Generic)

instance FromJSON SendMessageResult where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldToCamelWithoutPrefix "sendMessage"
        }

data LongPollServerResponse = LongPollServerResponse
  { longPollServerResponseKey :: String,
    longPollServerResponseServer :: String,
    longPollServerResponseTs :: String
  }
  deriving (Generic, Show)

instance FromJSON LongPollServerResponse where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier =
            jsonFieldToCamelWithoutPrefix "longPollServerResponse"
        }

newtype LongPollServer = LongPollServer
  { longPollServerResponse :: LongPollServerResponse
  }
  deriving (Generic, Show)

instance FromJSON LongPollServer where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldToCamelWithoutPrefix "longPollServer"
        }

data VkUpdateObject = VkUpdateObject
  { vkUpdateObjectId :: Int,
    vkUpdateObjectDate :: Int,
    vkUpdateObjectUserId :: Int,
    vkUpdateObjectTitle :: String,
    vkUpdateObjectBody :: String
  }
  deriving (Generic, Show)

instance FromJSON VkUpdateObject where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldToSnakeWithoutPrefix "vkUpdateObject"
        }

data VkUpdate = VkUpdate
  { vkUpdateType :: String,
    vkUpdateObject :: VkUpdateObject,
    vkUpdateGroupId :: Int
  }
  deriving (Generic, Show)

instance FromJSON VkUpdate where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldToSnakeWithoutPrefix "vkUpdate"
        }

data VkUpdates = VkUpdates
  { vkUpdatesTs :: String,
    vkUpdatesUpdates :: [VkUpdate]
  }
  deriving (Generic, Show)

instance FromJSON VkUpdates where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = jsonFieldToSnakeWithoutPrefix "vkUpdates"
        }

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
