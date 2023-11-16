module Test.TestContainers
  ( GenericContainer
  , WaitStrategy(..)
  , Environment(..)
  , mkGenericContainer
  , stop
  , start
  , containerHost
  , containerPort
  , exposePort
  , waitStrategy
  , setEnvironment
  ) where

import Prelude

import Control.Monad.Except (throwError)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, error)

foreign import data Container :: Type

data WaitStrategy
  = LogWaitStrategy String (Maybe Int)
  | StartupTimeout Int

data GenericContainer
  = StartedGenericContainer Container
  | StoppedGenericContainer Container

type GCConstructor = (Container -> GenericContainer)
type Environment = { key :: String, value :: String }

-- TestContainers wrapper
foreign import mkGenericContainerImpl :: GCConstructor -> String -> Effect GenericContainer
foreign import startImpl :: GCConstructor -> Container -> Effect (Promise GenericContainer)
foreign import stopImpl :: GCConstructor -> Container -> Effect (Promise GenericContainer)
foreign import exposePortImpl :: GCConstructor -> Container -> Int -> Effect GenericContainer
foreign import containerHostImpl :: Container -> Effect String
foreign import containerPortImpl :: Container -> Int -> Effect Int
foreign import waitStrategyImpl :: Container -> WaitStrategy -> Effect Unit
foreign import setEnvironmentImpl :: Container -> Array Environment -> Effect Unit

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

waitStrategy :: GenericContainer -> WaitStrategy -> Effect GenericContainer
waitStrategy (StartedGenericContainer _) _ = throwError $ error "Cannot use this method on a running container"
waitStrategy x@(StoppedGenericContainer c) ws = do 
  waitStrategyImpl c ws
  pure x

setEnvironment :: GenericContainer -> Array Environment -> Effect GenericContainer
setEnvironment x@(StoppedGenericContainer c) env = do
  setEnvironmentImpl c env
  pure x

setEnvironment _ _ = throwError $ error "Cannot use this method on a running container"
