{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TypesSpec
  ( spec,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust, isJust)
import Test.Hspec
import Text.RawString.QQ
import qualified Types as T

spec :: Spec
spec = do
  describe "Updates" $ do
    it "Should deserialize Updates json" $ do
      let actual = Aeson.decode updatesJson :: Maybe T.Updates
      isJust actual `shouldBe` True
    it "Should deserialize Updates json properly" $ do
      let updates = T.updatesResult . fromJust . Aeson.decode $ updatesJson :: [T.Update]
      let editedUpdate = head updates
      (T.tgMessageType . T.message $ editedUpdate) `shouldBe` T.EditedMessage

      let update = updates !! 1
      (T.tgMessageType . T.message $ update) `shouldBe` T.NewMessage

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