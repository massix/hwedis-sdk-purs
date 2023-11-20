module Test.Configuration where

import Prelude

import Data.Configuration as Config
import Data.Either (Either(..), isLeft, isRight)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Assertions.String (shouldStartWith)

testConfiguration :: Spec Unit
testConfiguration = do
  describe "Parse Configuration" do
    it "should be able to parse configuration from file" do
      allRight <- liftEffect $ Config.parse "./test/sample_conf.toml"

      allRight `shouldSatisfy` isRight

      let config = unsafePartial $ unsafeRight allRight
      config.workers `shouldEqual` 3
      config.db.host `shouldEqual` "fauxdb"
      config.db.user `shouldEqual` "configRead"
      config.hwedis.clientPrefix `shouldEqual` "fromConfig"

    it "should fail gracefully if the file does not exist" do
      allLeft <- liftEffect $ Config.parse "./test/doesnotexist.toml"
      allLeft `shouldSatisfy` isLeft

      let error = unsafePartial $ unsafeLeft allLeft
      error `shouldStartWith` "ENOENT: no such file or directory"

    it "should fail gracefully if the configuration is malformed" do
      allLeft <- liftEffect $ Config.parse "./test/sample_invalid.toml"
      allLeft `shouldSatisfy` isLeft

      let error = unsafePartial $ unsafeLeft allLeft
      error `shouldStartWith` "An error occurred while decoding a JSON value:"

  where
  unsafeRight :: ∀ a b. Partial => Either a b -> b
  unsafeRight (Right y) = y

  unsafeLeft :: ∀ a b. Partial => Either a b -> a
  unsafeLeft (Left y) = y
