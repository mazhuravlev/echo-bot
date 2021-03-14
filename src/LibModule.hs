{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LibModule where

import Data.Aeson
import Data.Char (isUpper)
import qualified Data.Text as Text
import Data.Text.Manipulate (toCamel)
import Dhall

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
    genericParseJSON defaultOptions {fieldLabelModifier = jsonFieldRemovePrefix}

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
    genericParseJSON defaultOptions {fieldLabelModifier = jsonFieldRemovePrefix}

data TelegramConfig =
  TelegramConfig
    { telegramToken :: String
    , telegramTimeout :: Natural
    }
  deriving (Generic, Show)

instance FromDhall TelegramConfig

jsonFieldRemovePrefix :: [Char] -> [Char]
jsonFieldRemovePrefix =
  Text.unpack . toCamel . Text.pack . dropWhile (not . isUpper)
