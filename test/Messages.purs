module Test.Messages where

import Prelude

import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Messages as M
import Data.String (take)
import Effect.Aff (Aff)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

testMessages :: SpecT Aff Unit Identity Unit
testMessages = do
  describe "Test Requests" do
    it "should render message to string" do
      show M.List `shouldEqual` "(List)"
      show (M.Get "1234") `shouldEqual` "(Get 1234)"
      show 
        (M.Create "1234" 
          [ mkField "field1" "value1"
          , mkField "field2" "value2"
          ]) `shouldEqual` "(Create 1234 [field1: value1], [field2: value2])"
      show (M.Update "1234" []) `shouldEqual` "(Update 1234 )"
      show (M.Delete "1234") `shouldEqual` "(Delete 1234)"

    it "should render message to a format understood by hwedis" do
      M.toWsMessage M.List `shouldEqual` "L"
      M.toWsMessage (M.Get "objectid") `shouldEqual` "G::objectid"
      M.toWsMessage (M.Create "obj" [mkField "field" "value"]) `shouldEqual` "C::obj::field::value"
      M.toWsMessage 
        (M.Update "objectid" 
          [ mkField "field1" "value1"
          , mkField "field2" "value2"
          , mkField "field3" "value3"
          ]) `shouldEqual` "U::objectid::field1::value1::field2::value2::field3::value3"
      M.toWsMessage (M.Delete "1234") `shouldEqual` "D::1234"

    it "should validate the messages" do
      let validGet = M.Get "someobje"
          invalidGet = M.Get "short"
          validDelete = M.Delete "someobje"
          invalidDelete = M.Delete "objectidtoolong"
          validCreate = M.Create "someobje" [mkField "field" "value"]
          invalidCreate = M.Create "someobje" [mkField "fieldnameisdefinitelytool" "value"]
          validUpdate = M.Update "someobje" [mkField "field" "value"]
          veryLongValue -- sorry mom
            = "nowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthisisaverylongvaluenowthi"
          invalidUpdate = M.Update "someobje" [ mkField "field" "value"
                                              , mkField "field" veryLongValue
                                              ]

      M.validateRequest M.List `shouldEqual` (Just M.List)
      M.validateRequest validGet `shouldEqual` (Just validGet)
      M.validateRequest invalidGet `shouldEqual` Nothing
      M.validateRequest validDelete `shouldEqual` (Just validDelete)
      M.validateRequest invalidDelete `shouldEqual` Nothing
      M.validateRequest validCreate `shouldEqual` (Just validCreate)
      M.validateRequest invalidCreate `shouldEqual` Nothing
      M.validateRequest validUpdate `shouldEqual` (Just validUpdate)
      M.validateRequest invalidUpdate `shouldEqual` Nothing

      -- I can mapR on the objectId to truncate it to 8 characters
      M.validateRequest (M.mapR (take 8) invalidDelete) `shouldEqual` Just (M.Delete "objectid")

  where
    mkField :: String -> String -> M.Field
    mkField = { key: _, value: _ }

