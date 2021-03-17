module LibModuleSpec
  ( spec
  ) where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI, uriAuthority, uriQuery, uriRegName)
import Test.Hspec
import qualified LibModule as L
import qualified Types as T
import qualified Data.ByteString.Char8 as BS

testTelegramConfig :: T.TelegramConfig
testTelegramConfig =  T.TelegramConfig {T.telegramToken = "TOKEN", T.telegramTimeout = 3}

tgGen :: L.TgApiUrlGen
tgGen = L.mkTgApiUrlGen testTelegramConfig

assertTgAuthority :: URI -> Expectation
assertTgAuthority =
  (`shouldBe` "api.telegram.org") . uriRegName . fromJust . uriAuthority

assertQuery :: [Char] -> [[Char]] -> Expectation
assertQuery query = (`shouldBe` True) . all (`elem` queryParts)
  where
    queryParts = splitOn "&" . drop 1 $ query

spec :: Spec
spec = do
  describe "TgApiUrlGen" $ do
    it "Should generate valid 'getUpdates' URL" $ do
      let uri = fromJust . parseURI $ L.getUpdatesUrl tgGen 8
      assertTgAuthority uri
    it "Should generate valid 'sendMessage' URL" $ do
      let uri = fromJust . parseURI $ L.sendMessageUrl tgGen 8 "message!"
      assertTgAuthority uri
      assertQuery (uriQuery uri) ["text=message!", "chat_id=8"]
  describe "Decode VK message.send result" $ do
    it "Should return message id of successful result" $ do
      let actual = L.parseVkMessageSendResult (BS.pack "{\"response\":21}")
      actual `shouldBe` Right 21
    it "Should return error of failed result" $ do
      let actual = L.parseVkMessageSendResult (BS.pack "{\"error\":{\"error_code\":100,\"error_msg\":\"One of the parameters specified was missing or invalid\"}}")
      actual `shouldBe` Left "One of the parameters specified was missing or invalid; error code: 100"
  describe "tgNextOffset" $ do
    it "Should return original offset if update list is empty" $ do
      L.tgNextOffset 3 [] `shouldBe` 3
    it "Should return updates max offset incremented by 1" $ do
      L.tgNextOffset 3 [T.Update 123 undefined, T.Update 234 undefined] `shouldBe` 235