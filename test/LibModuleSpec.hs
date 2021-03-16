module LibModuleSpec
  ( spec
  ) where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Network.URI (URI, parseURI, uriAuthority, uriQuery, uriRegName)
import Test.Hspec
import LibModule
import qualified Types as T

testTelegramConfig :: T.TelegramConfig
testTelegramConfig =  T.TelegramConfig {T.telegramToken = "TOKEN", T.telegramTimeout = 3}

tgGen :: TgApiUrlGen
tgGen = mkTgApiUrlGen testTelegramConfig

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
      let uri = fromJust . parseURI $ getUpdates tgGen 8
      assertTgAuthority uri
    it "Should generate valid 'sendMessage' URL" $ do
      let uri = fromJust . parseURI $ sendMessage tgGen 8 "message!"
      assertTgAuthority uri
      assertQuery (uriQuery uri) ["text=message!", "chat_id=8"]
