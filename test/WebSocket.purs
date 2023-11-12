module Test.WebSocket (testWebSocket) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Network.WebSocket as WS
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

foreign import data Container :: Type

data GenericContainer
  = StartedGenericContainer Container
  | StoppedGenericContainer Container

type GCConstructor = (Container -> GenericContainer)

-- TestContainers wrapper
foreign import mkGenericContainerImpl :: GCConstructor -> String -> Effect GenericContainer
foreign import startImpl :: GCConstructor -> Container -> Effect (Promise GenericContainer)
foreign import stopImpl :: GCConstructor -> Container -> Effect (Promise GenericContainer)
foreign import exposePortImpl :: GCConstructor -> Container -> Int -> Effect GenericContainer
foreign import containerHostImpl :: Container -> Effect String
foreign import containerPortImpl :: Container -> Int -> Effect Int

-- PURS Implementation
mkGenericContainer :: String -> Effect GenericContainer
mkGenericContainer image = mkGenericContainerImpl StoppedGenericContainer image

stop :: GenericContainer -> Aff GenericContainer
stop (StartedGenericContainer c) = toAffE $ stopImpl StoppedGenericContainer c
stop x = pure x

start :: GenericContainer -> Aff GenericContainer
start (StoppedGenericContainer c) = toAffE $ startImpl StartedGenericContainer c
start x = pure x

containerHost :: GenericContainer -> Effect (Maybe String)
containerHost (StartedGenericContainer c) = containerHostImpl c >>= (pure <<< Just)
containerHost _ = pure Nothing

containerPort :: GenericContainer -> Int -> Effect (Maybe Int)
containerPort (StartedGenericContainer c) p = containerPortImpl c p >>= (pure <<< Just)
containerPort _ _ = pure Nothing

exposePort :: GenericContainer -> Int -> Effect GenericContainer
exposePort (StoppedGenericContainer x) port = exposePortImpl StoppedGenericContainer x port
exposePort _ _ = throwError $ error "Cannot expose a port of a started container"

testWebSocket :: SpecT Aff Unit Identity Unit
testWebSocket = do
  describe "WebSocket Tests" do
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
