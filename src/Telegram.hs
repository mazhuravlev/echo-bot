{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Telegram where

import Control.Monad.State
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Dhall (Generic)
import qualified LibModule as L

data CommpandOp = Repeat Int | ReplyOnce | Reply

type TgState m = (L.Logger m, Api m, L.UserMap)

type ApiUrl = String

type ChatId = Int

data Api m = Api
  { getUpdates :: Int -> m (Either String [Update]),
    sendMessage :: ChatId -> String -> m (Either String String)
  }

data ApiUrlGen = ApiUrlGen
  { getUpdatesUrl :: Int -> ApiUrl,
    sendMessageUrl :: ChatId -> String -> ApiUrl
  }

processMessage :: Monad m => L.Logger m -> Api m -> L.UserMap -> (Int, String) -> m L.UserMap
processMessage logger tgApi userMap (chatid, messageText) = do
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
      sendResult <- sendMessage tgApi chatid msg'
      case sendResult of
        Left err -> L.logError logger $ "Telegram.sendMessage FAIL decoding json response: " ++ err
        Right _ -> L.logInfo logger $ "Telegram.sendMessage OK chat_id " ++ show chatid

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

getState :: Monad m => StateT (TgState m) m (TgState m)
getState = do get

getLogger :: Monad m => StateT (TgState m) m (L.Logger m)
getLogger = do
  (l, _, _) <- getState
  return l

loop :: Monad m => Int -> StateT (TgState m) m ()
loop offset = do
  (_, api, userMap) <- get
  logger <- getLogger
  eitherUpdates <- lift (getUpdates api offset)
  case eitherUpdates of
    Right updates -> do
      lift ((if null updates then L.logDebug else L.logInfo) logger $ formatUpdatesLog updates)
      userMap' <- lift (foldM (processMessage logger api) userMap $ getChats updates)
      put (logger, api, userMap')
      loop (nextOffset offset updates)
    Left err -> do
      lift (L.logError logger $ "Failed to get updates: " ++ err)
      loop offset
  return ()
  where
    formatUpdatesLog updates' =
      "Received "
        ++ (show . length $ updates')
        ++ " Telegram updates"

mkApi :: Monad m => L.TelegramConfig -> (String -> m BS.ByteString) -> Api m
mkApi config httpReq = Api {getUpdates = getUpdatesFn, sendMessage = sendMessageFn}
  where
    getUpdatesFn offset = do
      updatesJson <- httpReq $ getUpdatesUrl urlGen offset
      return $ case eitherDecode (LBS.fromStrict updatesJson) :: Either String Updates of
        Left err -> jsonDecodeError err updatesJson
        Right updates@(updatesOk -> True) -> Right . updatesResult $ updates
        _ -> Left "Unexpected error occured on receveing updates"
    sendMessageFn cid msg = do
      messageResultjson <- httpReq $ sendMessageUrl urlGen cid msg
      return $ case eitherDecode (LBS.fromStrict messageResultjson) :: Either String SendMessageResult of
        Left err -> jsonDecodeError err messageResultjson
        Right (sendMessageOk -> True) -> Right $ "Message sent to chat " ++ show cid
        _ -> Left $ "Failed to send message to chat " ++ show cid
    urlGen = mkApiUrlGen config
    jsonDecodeError err json = Left $ "Response JSON decode error: " ++ err ++ show json

getChats :: [Update] -> [(Int, String)]
getChats = map ((\x -> (chatId . chat $ x, text x)) . message)

nextOffset :: Int -> [Update] -> Int
nextOffset offset [] = offset
nextOffset _ updates = (+ 1) . foldr (max . update_id) 0 $ updates

mkApiUrlGen :: L.TelegramConfig -> ApiUrlGen
mkApiUrlGen config =
  ApiUrlGen {getUpdatesUrl = getUpdatesFn, sendMessageUrl = sendMessageFn}
  where
    tgBaseUrl = "https://api.telegram.org/bot" ++ L.telegramToken config
    getUpdatesFn offset =
      concat
        [ tgBaseUrl,
          "/getUpdates?offset=",
          show offset,
          "&timeout=" ++ show (L.telegramTimeout config)
        ]
    sendMessageFn chatid messageText =
      concat
        [tgBaseUrl, "/sendMessage?chat_id=", show chatid, "&text=", messageText]

data Chat = Chat
  { chatId :: Int,
    chatUsername :: String
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON Chat where
  parseJSON =
    genericParseJSON
      defaultOptions {fieldLabelModifier = L.jsonFieldToCamelWithoutPrefix "chat"}

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
        { fieldLabelModifier = L.jsonFieldToCamelWithoutPrefix "updates"
        }

newtype SendMessageResult = SendMessageResult
  { sendMessageOk :: Bool
  }
  deriving (Show, Generic)

instance FromJSON SendMessageResult where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = L.jsonFieldToCamelWithoutPrefix "sendMessage"
        }