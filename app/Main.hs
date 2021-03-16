{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Dhall (auto, input)
import GHC.IO.Handle.FD (stderr)
import LibModule
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.IO (hPutStrLn)

-- TODO: add configurable timeout for longpolling support
fetchJSON :: String -> IO BS.ByteString
fetchJSON url = do
  req <- parseRequest url
  res <- httpBS req
  return (getResponseBody res)

tgSendEcho :: Logger IO -> TgApiUrlGen -> (Int, String) -> IO ()
tgSendEcho logger urlGen (chatid, messageText) = do
  json <- fetchJSON $ sendMessage urlGen chatid messageText
  case eitherDecode (LBS.fromStrict json) :: Either String SendMessageResult of
    Right (sendMessageOk -> True) ->
      logInfo logger $ "Telegram.sendMessage OK chat_id " ++ show chatid
    Right _ ->
      logError logger $ "Telegram.sendMessage FAIL chat_id " ++ show chatid
    Left err ->
      logError logger $
      "Telegram.sendMessage FAIL decoding json response: " ++ err
  return ()

tgLoop :: Logger IO -> TgApiUrlGen -> Int -> IO ()
tgLoop logger urlGen offset = do
  json <- fetchJSON $ getUpdates urlGen offset
  case eitherDecode (LBS.fromStrict json) :: Either String Updates of
    Right updates -> do
      logReceivedUpdates updates
      mapM_ (tgSendEcho logger urlGen) $ getChats updates
      tgLoop logger urlGen $ getNextOffset updates
    Left err -> logError logger $ "Failed to get updates: " ++ err
  where
    getChats =
      map ((\x -> (chatId . chat $ x, text x)) . message) . updatesResult
    getNextOffset =
      maybe offset (+ 1) . listToMaybe . map update_id . updatesResult
    logReceivedUpdates updates' =
      logDebug logger $
      "Received " ++
      (show . length . updatesResult $ updates') ++ " Telegram updates"

type VkApiMethod = String

type VkApiParam = (String, String)

type VkUrlGen = (VkApiMethod -> [VkApiParam] -> VkApiUrl)

startVkLoop :: Logger IO -> VkUrlGen -> String -> IO ()
startVkLoop logger urlGen groupId = do
  json <- fetchJSON $ urlGen "groups.getLongPollServer" [("group_id", groupId)]
  case eitherDecode (LBS.fromStrict json) :: Either String LongPollServer of
    Right longPollServer -> do
      logInfo logger "Received VK longpoll server info"
      let lpsr = longPollServerResponse longPollServer
      let mkLpUrl =
            vkLongpoll
              (longPollServerResponseServer lpsr)
              (longPollServerResponseKey lpsr)
              "20" -- TODO: parametrize timeout
      vkLoop mkLpUrl (longPollServerResponseTs lpsr)
    Left err -> logError logger $ "VK groups.getLongPollServer FAILED:\n" ++ err
  where
    vkLoop mkUrl ts = do
      lpJson <- fetchJSON $ mkUrl ts
      case eitherDecode (LBS.fromStrict lpJson) :: Either String VkUpdates of
        Right updates -> do
          logInfo logger $
            "VK receive updates OK, ts = " ++ show (vkUpdatesTs updates)
          vkLoop mkUrl (vkUpdatesTs updates)
        Left err -> do
          logError logger $ "VK receive updates FAIL:\n" ++ err ++ "\n"
          threadDelay 5000000
          vkLoop mkUrl ts

printStderr :: String -> IO ()
printStderr =hPutStrLn stderr

logFn :: LogLevel -> String -> IO ()
logFn level 
  | level == ErrorLogLevel = printStderr
  | otherwise  = putStrLn

main :: IO ()
main = do
  maybeBotConfig <-
    try (input auto "./config.dhall") :: IO (Either SomeException BotConfig)
  case maybeBotConfig of
    Right config -> do
      let logger = mkLogger logFn (botLogLevel config)
      logInfo logger "Bot started"
      if runVkBot config
        then do
          logInfo logger "Starting VK Bot"
          let vkConf = vkConfig config
           in startVkLoop
                logger
                (genVkApiUrl $ vkToken vkConf)
                (vkGroupId vkConf)
        else do
          logInfo logger "Starting Telegtam Bot"
          tgLoop logger (mkTgApiUrlGen . telegramConfig $ config) 0
    Left err -> do
      printStderr $ "BotConfig read error:\n" ++ show err
