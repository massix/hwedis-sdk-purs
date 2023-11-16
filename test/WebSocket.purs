module Test.WebSocket (testWebSocket) where

import Prelude

import Data.Identity (Identity)
import Data.Maybe (Maybe, fromMaybe, isNothing)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Network.WebSocket as WS
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.TestContainers (GenericContainer, containerHost, containerPort, exposePort, mkGenericContainer, start, stop)

testWebSocket :: SpecT Aff Unit Identity Unit
testWebSocket = do
  describe "WebSocket" do
    it "should use websockets to communicate with an echo server" do
      container <- liftEffect $ (mkGenericContainer wsEcho) >>= (flip exposePort) 3000
      started <- liftAff $ start container
      { host, port } <- liftEffect $ extractHostPort started

      when (isNothing host || isNothing port)
        $ throwError
        $ error "Could not start container"

      ws <- WS.mkWebSocket (mkHost (fromMaybe "" host) (fromMaybe 0 port)) "nodejs-test"
      WS.send ws "hello world of ffi"
      rec <- WS.recv ws
      void $ WS.close ws 1000 "Goodbye!"
      void $ stop started

      rec `shouldEqual` "hello world of ffi"

    it "should close the connection in a proper way" do
      container <- liftEffect $ (mkGenericContainer wsEcho) >>= (flip exposePort) 3000
      started <- liftAff $ start container
      { host, port } <- liftEffect $ extractHostPort started

      when (isNothing host || isNothing port)
        $ throwError
        $ error "Could not start container"

      ws <- WS.mkWebSocket (mkHost (fromMaybe "" host) (fromMaybe 0 port)) "nodejs-test"
      code <- WS.close ws 1000 "Goodbye!"

      code `shouldEqual` 1000

  where
  mkHost :: String -> Int -> String
  mkHost host port = "ws://" <> host <> ":" <> show port

  wsEcho :: String
  wsEcho = "elegantmonkeys/websockets-demo"

  extractHostPort :: GenericContainer -> Effect { host :: Maybe String, port :: Maybe Int }
  extractHostPort cnt = do
    host <- containerHost cnt
    port <- containerPort cnt 3000
    pure { host, port }
