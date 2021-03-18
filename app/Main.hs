{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

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
import qualified Telegram
import qualified Vk

-- TODO: add configurable timeout for longpolling support
fetchJSON :: String -> IO (Status, BS.ByteString)
fetchJSON url = do
  req <- Http.parseRequest url
  res <- Http.httpBS req
  return (Http.getResponseStatus res, Http.getResponseBody res)

main :: IO ()
main = do
  maybeBotConfig <-
    try (input auto "./config.dhall") :: IO (Either SomeException L.BotConfig)
  case maybeBotConfig of
    Right config -> do
      let logger = L.mkLogger logFn (L.botLogLevel config) Data.Time.getCurrentTime
      L.logInfo logger $ "Starting " ++ show (L.botType config) ++ " bot"
      case L.botType config of
        L.VK -> do
          let vkConf = L.vkConfig config
           in Vk.loop
                logger
                (Vk.genVkApiUrl $ L.vkToken vkConf)
                (L.vkGroupId vkConf)
        L.Telegram -> do
          evalStateT (Telegram.loop 0) (logger, Telegram.mkApi (L.telegramConfig config) httpFn, Map.empty)
    Left err -> do
      printStderr $ "BotConfig read error:\n" ++ show err
  where
    logFn level
      | level == L.ErrorLogLevel = printStderr
      | otherwise = putStrLn
    printStderr = hPutStrLn stderr
    httpFn = fmap snd <$> fetchJSON
