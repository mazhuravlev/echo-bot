{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad.State
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import Data.Time (getCurrentTime)
import Dhall (auto, input)
import GHC.IO.Handle.FD (stderr)
import qualified LibModule as L
import Network.HTTP.Simple (getResponseBody, getResponseStatus, httpBS, parseRequest)
import Network.HTTP.Types (Status)
import System.IO (hPutStrLn)
import System.Random (randomIO)
import qualified Types as T

-- TODO: add configurable timeout for longpolling support
fetchJSON :: String -> IO (Status, BS.ByteString)
fetchJSON url = do
  req <- parseRequest url
  res <- httpBS req
  return (getResponseStatus res, getResponseBody res)

type TgState m = (L.Logger m, L.TgApi m, L.UserMap)

tgState :: Monad m => StateT (TgState m) m (TgState m)
tgState = do get

tgLogger :: Monad m => StateT (TgState m) m (L.Logger m)
tgLogger = do
  (l, _, _) <- tgState
  return l

tgLoop :: Int -> StateT (TgState IO) IO ()
tgLoop offset = do
  (_, api, userMap) <- get
  logger <- tgLogger
  eitherUpdates <- lift (L.tgGetUpdates api offset)
  case eitherUpdates of
    Right updates -> do
      lift ((if null updates then L.logDebug else L.logInfo) logger $ formatUpdatesLog updates)
      userMap' <- lift (foldM (L.tgProcessMessage logger api) userMap $ L.tgGetChats updates)
      put (logger, api, userMap')
      tgLoop (L.tgNextOffset offset updates)
    Left err -> lift (L.logError logger $ "Failed to get updates: " ++ err)
  return ()
  where
    formatUpdatesLog updates' =
      "Received "
        ++ (show . length $ updates')
        ++ " Telegram updates"

vkSendEcho :: L.Logger IO -> L.VkUrlGen -> T.VkUpdate -> IO ()
vkSendEcho logger urlGen update = do
  rnd <- randomIO :: IO Int
  (_, json) <-
    fetchJSON $
      urlGen
        "messages.send"
        [ ("group_id", show $ T.vkUpdateGroupId update),
          ("peer_id", show . T.vkUpdateObjectUserId . T.vkUpdateObject $ update),
          ("message", T.vkUpdateObjectBody . T.vkUpdateObject $ update),
          ("random_id", show rnd)
        ]
  case L.parseVkMessageSendResult json of
    Right msgId -> L.logInfo logger $ "VK mesage send OK: " ++ show msgId
    Left err -> L.logError logger $ "VK message send FAIL: " ++ err

startVkLoop :: L.Logger IO -> L.VkUrlGen -> String -> IO ()
startVkLoop logger urlGen groupId = do
  (_, json) <- fetchJSON $ urlGen "groups.getLongPollServer" [("group_id", groupId)]
  case eitherDecode (LBS.fromStrict json) :: Either String T.LongPollServer of
    Right longPollServer -> do
      L.logInfo logger "Received VK longpoll server info"
      let lpsr = T.longPollServerResponse longPollServer
      let mkLpUrl =
            L.vkLongpoll
              (T.longPollServerResponseServer lpsr)
              (T.longPollServerResponseKey lpsr)
              "20" -- TODO: parametrize timeout
      vkLoop mkLpUrl (T.longPollServerResponseTs lpsr)
    Left err -> L.logError logger $ "VK groups.getLongPollServer FAILED:\n" ++ err
  where
    vkLoop mkUrl ts = do
      (_, lpJson) <- fetchJSON $ mkUrl ts
      case eitherDecode (LBS.fromStrict lpJson) :: Either String T.VkUpdates of
        Right updates -> do
          L.logInfo logger $
            "VK receive updates OK, ts = " ++ show (T.vkUpdatesTs updates)
          mapM_ (vkSendEcho logger urlGen) (T.vkUpdatesUpdates updates)
          vkLoop mkUrl (T.vkUpdatesTs updates)
        Left err -> do
          L.logError logger $ "VK receive updates FAIL:\n" ++ err ++ "\n"
          threadDelay 5000000
          vkLoop mkUrl ts

printStderr :: String -> IO ()
printStderr = hPutStrLn stderr

logFn :: T.LogLevel -> String -> IO ()
logFn level
  | level == T.ErrorLogLevel = printStderr
  | otherwise = putStrLn

main :: IO ()
main = do
  maybeBotConfig <-
    try (input auto "./config.dhall") :: IO (Either SomeException T.BotConfig)
  case maybeBotConfig of
    Right config -> do
      let logger = L.mkLogger logFn (T.botLogLevel config) getCurrentTime
      L.logInfo logger "Bot started"
      if T.runVkBot config
        then do
          L.logInfo logger "Starting VK Bot"
          let vkConf = T.vkConfig config
           in startVkLoop
                logger
                (L.genVkApiUrl $ T.vkToken vkConf)
                (T.vkGroupId vkConf)
        else do
          L.logInfo logger "Starting Telegtam Bot"
          evalStateT (tgLoop 0) (logger, L.mkTgApi (T.telegramConfig config) (fmap snd <$> fetchJSON), Map.empty)
    Left err -> do
      printStderr $ "BotConfig read error:\n" ++ show err
