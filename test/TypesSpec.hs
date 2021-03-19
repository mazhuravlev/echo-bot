{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TypesSpec
  ( spec,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromJust, isJust)
import Test.Hspec
import Text.RawString.QQ
import qualified Telegram as Tg

spec :: Spec
spec = do
  describe "Updates" $ do
    it "Should deserialize Updates json" $ do
      let actual = Aeson.decode updatesJson :: Maybe Tg.Updates
      isJust actual `shouldBe` True
    it "Should deserialize Updates json properly" $ do
      let updates = Tg.updatesResult . fromJust . Aeson.decode $ updatesJson :: [Tg.Update]
      let editedUpdate = head updates
      (Tg.tgMessageType . Tg.message $ editedUpdate) `shouldBe` Tg.EditedMessage

      let update = updates !! 1
      (Tg.tgMessageType . Tg.message $ update) `shouldBe` Tg.NewMessage

updatesJson :: LBS.ByteString
updatesJson =
  [r|
{
   "ok":true,
   "result":[
      {
         "update_id":935643743,
         "edited_message":{
            "message_id":182,
            "from":{
               "id":194153091,
               "is_bot":false,
               "first_name":"Mikhail",
               "last_name":"Zhuravlev",
               "username":"mazhuravlev",
               "language_code":"en"
            },
            "chat":{
               "id":194153091,
               "first_name":"Mikhail",
               "last_name":"Zhuravlev",
               "username":"mazhuravlev",
               "type":"private"
            },
            "date":1616003729,
            "edit_date":1616003732,
            "text":"/repeat 1",
            "entities":[
               {
                  "offset":0,
                  "length":7,
                  "type":"bot_command"
               }
            ]
         }
      },
      {
         "update_id":935643744,
         "message":{
            "message_id":184,
            "from":{
               "id":194153091,
               "is_bot":false,
               "first_name":"Mikhail",
               "last_name":"Zhuravlev",
               "username":"mazhuravlev",
               "language_code":"en"
            },
            "chat":{
               "id":194153091,
               "first_name":"Mikhail",
               "last_name":"Zhuravlev",
               "username":"mazhuravlev",
               "type":"private"
            },
            "date":1616003736,
            "text":"/repeat 1",
            "entities":[
               {
                  "offset":0,
                  "length":7,
                  "type":"bot_command"
               }
            ]
         }
      }
   ]
}
|]