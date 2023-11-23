module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Configuration (testConfiguration)
import Test.Database (testDatabase)
import Test.Messages (testMessages)
import Test.Spec.Config (Config, defaultConfig)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec')
import Test.WebSocket (testWebSocket)

main :: Effect Unit
main = launchAff_ $ runSpec' testConfig [ consoleReporter ] do
  testMessages
  testWebSocket
  testDatabase
  testConfiguration

  where
  testConfig :: Config
  testConfig = defaultConfig { timeout = Just (Milliseconds 10_000.0), failFast = true }

