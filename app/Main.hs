{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, try)
import Control.Monad.State (evalStateT)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import qualified Data.Time
import Dhall (auto, input)
import GHC.IO.Handle.FD (stderr)
import qualified LibModule as L
import qualified Network.HTTP.Simple as Http
import Network.HTTP.Types (Status)
import System.IO (hPutStrLn)
import System.Random (randomIO)
import qualified Telegram
import qualified Vk

main :: IO ()
main = do
  maybeBotConfig <-
    try (input auto "./config.dhall") :: IO (Either SomeException L.BotConfig)
  case maybeBotConfig of
    Right config -> do
      let logger = L.mkLogger logFn (L.botLogLevel config) Data.Time.getCurrentTime
      let messageProcessor = L.mkMessageProcessor (L.helpMessage config) (L.defaultRepeatCount config)
      L.logInfo logger $ "Starting " ++ show (L.botType config) ++ " bot"
      case L.botType config of
        L.Vk -> do
          (api, initOffset) <- Vk.mkApi logger (L.vkConfig config) httpFn randomIO
          evalStateT (Vk.loop initOffset) (logger, api, messageProcessor, Map.empty)
        L.Telegram -> do
          evalStateT (Telegram.loop 0) (logger, Telegram.mkApi (L.telegramConfig config) httpFn, messageProcessor, Map.empty)
    Left err -> do
      printStderr $ "BotConfig read error:\n" ++ show err
  where
    logFn level
      | level == L.ErrorLogLevel = printStderr
      | otherwise = putStrLn
    printStderr = hPutStrLn stderr
    httpFn = fmap snd <$> fetchJSON

-- TODO: add configurable timeout for long polling support
fetchJSON :: String -> IO (Status, BS.ByteString)
fetchJSON url = do
  req <- Http.parseRequest url
  res <- Http.httpBS req
  return (Http.getResponseStatus res, Http.getResponseBody res)