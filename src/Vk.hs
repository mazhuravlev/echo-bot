{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Vk where

import Control.Lens (preview)
import Control.Monad.State (StateT, get, lift)
import Data.Aeson (eitherDecode)
import Data.Aeson.Lens (AsNumber (_Number), key, _String)
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
--import qualified Data.Map as Map
import Data.Text (unpack)
import Dhall (Generic)
import qualified LibModule as L

type ApiUrl = String

type ApiMethod = String

type ApiParam = (String, String)

type UrlGen = (ApiMethod -> [ApiParam] -> ApiUrl)

type UserId = Int

type Message = String

type MessageId = Int

type GroupId = Int

type Token = String

type Method = String

type LPServer = String

type LPKey = String

type LPTimeout = String

type LPOffset = String

type LPUrl = String

type State m = (L.Logger m, Api m, L.UserMap)

data Api m = Api
  { getUpdates :: LPOffset -> m (Either String Updates),
    sendMessage :: (UserId, Message, GroupId) -> m (Either String MessageId)
  }

mkApi :: Monad m => L.Logger m -> L.VkConfig -> L.HttpR m -> m Int -> m (Api m, LPOffset)
mkApi logger config httpReq rnd = do
  let urlGen = genApiUrl (L.vkToken config)
  lprJson <- httpReq (urlGen "groups.getLongPollServer" [("group_id", L.vkGroupId config)])
  let lpE = eitherDecode . LBS.fromStrict $ lprJson
  case lpE of
    Right lpr -> do
      L.logInfo logger "Received VK longpoll server settings"
      let lp = longPollServerResponse lpr
      let lpUrlGen = makeLpUrlGen lp
      return (Api {getUpdates = getUpdatesFn lpUrlGen httpReq, sendMessage = sendMessageFn urlGen}, longPollServerResponseTs lp)
    Left err -> error $ "VK receive updates FAIL: " ++ err ++ " in json: " ++ show lprJson
  where
    sendMessageFn urlGen (userId, msg, groupId) = do
      rndId <- rnd
      let params =
            [ ("group_id", show groupId),
              ("peer_id", show userId),
              ("message", msg),
              ("random_id", show rndId)
            ]
      messageResult <-
        parseMessageSendResult
          <$> httpReq
            ( urlGen
                "messages.send"
                params
            )
      case messageResult of
        Right mid -> L.logInfo logger $ "Sent VK message with id: " ++ show mid
        Left err -> L.logError logger $ "Failed to send VK message: " ++ err ++ " with params: " ++ show params
      return messageResult
    getUpdatesFn lpUrlGen' http offset = eitherDecode . LBS.fromStrict <$> http (lpUrlGen' offset)
    makeLpUrlGen = getLongpollUrl <$> longPollServerResponseServer <*> longPollServerResponseKey <*> const "20"

loop :: Monad m => LPOffset -> StateT (State m) m ()
loop offset = do
  (logger, api, _) <- get
  updatesE <- lift (getUpdates api offset)
  case updatesE of
    Right updates -> do
      lift (logUpdates logger updates)
      lift (mapM_ (sendMessage api) $ getChats . vkUpdatesUpdates $ updates)
      loop (vkUpdatesTs updates)
    Left err -> do
      lift (L.logError logger $ "VK receive updates FAIL:\n" ++ err ++ "\n")
      -- TODO: handle error, retry after a timeout or stop if unauthorized
      error "VK stop"
  where
    logUpdates l u =
      let cnt = length . vkUpdatesUpdates $ u
          f = if cnt == 0 then L.logDebug else L.logInfo
       in f l ("Received " ++ show cnt ++ " VK updates")

getChats :: [Update] -> [(UserId, String, GroupId)]
getChats = map f
  where
    f = (,,) <$> vkUpdateObjectUserId . vkUpdateObject <*> vkUpdateObjectBody . vkUpdateObject <*> vkUpdateGroupId

genApiUrl :: Token -> Method -> [(String, String)] -> ApiUrl
genApiUrl token method params =
  concat ["https://api.vk.com/method/", method, "?", paramString]
  where
    paramString =
      intercalate "&" . map (\(k, v) -> k ++ "=" ++ v) $
        ("access_token", token) : ("v", "5.139") : params

getLongpollUrl :: LPServer -> LPKey -> LPTimeout -> LPOffset -> LPUrl
getLongpollUrl server skey timeout ts =
  concat [server, "?act=a_check&key=", skey, "&ts=", ts, "&wait=", timeout]

parseMessageSendResult :: BS.ByteString -> Either String Int
parseMessageSendResult json = maybe err Right okMaybe
  where
    okMaybe = round <$> preview (key "response" . _Number) json :: Maybe Int
    errorMsg = preview (key "error" . key "error_msg" . _String) json
    errorCode = round <$> preview (key "error" . key "error_code" . _Number) json :: Maybe Int
    errMaybe = (\m c -> unpack m ++ "; error code: " ++ show c) <$> errorMsg <*> errorCode
    err = maybe (Left $ "Unexpected error on json: " ++ show json) Left errMaybe

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
            L.jsonFieldToCamelWithoutPrefix "longPollServerResponse"
        }

newtype LongPollServer = LongPollServer
  { longPollServerResponse :: LongPollServerResponse
  }
  deriving (Generic, Show)

instance FromJSON LongPollServer where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = L.jsonFieldToCamelWithoutPrefix "longPollServer"
        }

data UpdateObject = UpdateObject
  { vkUpdateObjectId :: Int,
    vkUpdateObjectDate :: Int,
    vkUpdateObjectUserId :: Int,
    vkUpdateObjectTitle :: String,
    vkUpdateObjectBody :: String
  }
  deriving (Generic, Show)

instance FromJSON UpdateObject where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = L.jsonFieldToSnakeWithoutPrefix "vkUpdateObject"
        }

data Update = Update
  { vkUpdateType :: String,
    vkUpdateObject :: UpdateObject,
    vkUpdateGroupId :: Int
  }
  deriving (Generic, Show)

instance FromJSON Update where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = L.jsonFieldToSnakeWithoutPrefix "vkUpdate"
        }

data Updates = Updates
  { vkUpdatesTs :: String,
    vkUpdatesUpdates :: [Update]
  }
  deriving (Generic, Show)

instance FromJSON Updates where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = L.jsonFieldToSnakeWithoutPrefix "vkUpdates"
        }
