{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Vk where

import Control.Lens (preview)
import Data.Aeson.Lens (AsNumber (_Number), key, _String)
import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)
import Data.Text (unpack)
import Data.Aeson.Types
import Dhall (Generic)
import qualified LibModule as L
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (eitherDecode)
import System.Random (randomIO)
import Control.Concurrent (threadDelay)
import Network.HTTP.Simple (getResponseBody, getResponseStatus, httpBS, parseRequest)
import Network.HTTP.Types (Status)

-- TODO: !! REMOVE !!
fetchJSON :: String -> IO (Status, BS.ByteString)
fetchJSON url = do
  req <- parseRequest url
  res <- httpBS req
  return (getResponseStatus res, getResponseBody res)

type VkApiUrl = String

type VkApiMethod = String

type VkApiParam = (String, String)

type VkUrlGen = (VkApiMethod -> [VkApiParam] -> VkApiUrl)

vkSendEcho :: L.Logger IO -> VkUrlGen -> VkUpdate -> IO ()
vkSendEcho logger urlGen update = do
  rnd <- randomIO :: IO Int
  (_, json) <-
    fetchJSON $
      urlGen
        "messages.send"
        [ ("group_id", show $ vkUpdateGroupId update),
          ("peer_id", show . vkUpdateObjectUserId . vkUpdateObject $ update),
          ("message", vkUpdateObjectBody . vkUpdateObject $ update),
          ("random_id", show rnd)
        ]
  case parseVkMessageSendResult json of
    Right msgId -> L.logInfo logger $ "VK mesage send OK: " ++ show msgId
    Left err -> L.logError logger $ "VK message send FAIL: " ++ err

loop :: L.Logger IO -> VkUrlGen -> String -> IO ()
loop logger urlGen groupId = do
  (_, json) <- fetchJSON $ urlGen "groups.getLongPollServer" [("group_id", groupId)]
  case eitherDecode (LBS.fromStrict json) :: Either String LongPollServer of
    Right longPollServer -> do
      L.logInfo logger "Received VK longpoll server info"
      let lpsr = longPollServerResponse longPollServer
      let mkLpUrl =
            vkLongpoll
              (longPollServerResponseServer lpsr)
              (longPollServerResponseKey lpsr)
              "20" -- TODO: parametrize timeout
      vkLoop mkLpUrl (longPollServerResponseTs lpsr)
    Left err -> L.logError logger $ "VK groups.getLongPollServer FAILED:\n" ++ err
  where
    vkLoop mkUrl ts = do
      (_, lpJson) <- fetchJSON $ mkUrl ts
      case eitherDecode (LBS.fromStrict lpJson) :: Either String VkUpdates of
        Right updates -> do
          L.logInfo logger $
            "VK receive updates OK, ts = " ++ show (vkUpdatesTs updates)
          mapM_ (vkSendEcho logger urlGen) (vkUpdatesUpdates updates)
          vkLoop mkUrl (vkUpdatesTs updates)
        Left err -> do
          L.logError logger $ "VK receive updates FAIL:\n" ++ err ++ "\n"
          threadDelay 5000000
          vkLoop mkUrl ts


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

parseVkMessageSendResult :: BS.ByteString -> Either String Int
parseVkMessageSendResult json = maybe err Right okMaybe
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
        { fieldLabelModifier = L.jsonFieldToSnakeWithoutPrefix "vkUpdateObject"
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
        { fieldLabelModifier = L.jsonFieldToSnakeWithoutPrefix "vkUpdate"
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
        { fieldLabelModifier = L.jsonFieldToSnakeWithoutPrefix "vkUpdates"
        }
