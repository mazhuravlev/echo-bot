{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

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

printStderr :: String -> IO ()
printStderr = hPutStrLn stderr

main :: IO ()
main = do
  maybeBotConfig <-
    try (input auto "./config.dhall") :: IO (Either SomeException BotConfig)
  case maybeBotConfig of
    Right config -> do
      let logger = mkLogger printStderr putStrLn putStrLn (botLogLevel config)
      logInfo logger "Bot started"
      tgLoop logger (mkTgApiUrlGen config) 0
    Left err -> do
      printStderr $ "BotConfig read error:\n" ++ show err
