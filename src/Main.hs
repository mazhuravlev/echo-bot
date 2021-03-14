{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe)
import Dhall (auto, input)
import LibModule
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)

fetchJSON :: String -> IO BS.ByteString
fetchJSON url = do
  req <- parseRequest url
  res <- httpBS req
  return (getResponseBody res)

tgSendEcho :: TgApiUrlGen -> (Int, String) -> IO ()
tgSendEcho urlGen (chatid, messageText) = do
  _ <- fetchJSON $ sendMessage urlGen chatid messageText
  return ()

tgLoop :: TgApiUrlGen -> Int -> IO ()
tgLoop urlGen offset = do
  json <- fetchJSON $ getUpdates urlGen offset
  case eitherDecode (LBS.fromStrict json) :: Either String Updates of
    Right updates -> do
      let chats =
            map ((\x -> (chatId . chat $ x, text x)) . message) . updatesResult $
            updates
      mapM_ (tgSendEcho urlGen) chats
      let nextOffset =
            maybe offset (+ 1) . listToMaybe . map update_id . updatesResult $
            updates
      tgLoop urlGen nextOffset
    Left err -> putStrLn $ "Failed to get updates: " ++ err

main :: IO ()
main = do
  maybeTelegramConfig <-
    try (input auto "./config.dhall") :: IO (Either SomeException TelegramConfig)
  case maybeTelegramConfig of
    Right config -> do
      putStrLn "Bot started.."
      tgLoop (mkTgApiUrlGen config) 0
    Left err -> do
      putStrLn "TelegramConfig read error:"
      print err
