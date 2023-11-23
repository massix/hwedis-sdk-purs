module Monad.MonadCache where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Messages as Cache
import Data.Serializable (class Deserializable, class Serializable, deserialize, serialize)
import Effect.Aff.Class (class MonadAff, liftAff)
import Misc.Utils (padId)
import Network.WebSocket as WS

type PadLetter = String

-- This Monad can generate requests for the cache starting from an object s
class (MonadAff m) <= MonadCache m where
  getCacheConnection :: m WS.WebSocket

getCache :: ∀ a m. MonadCache m => Deserializable a => Int -> PadLetter -> m (Maybe a)
getCache id pl = do
  let
    paddedId = padId id <> pl
    request = Cache.Get paddedId

  ws <- getCacheConnection

  resp <- liftAff $ do
    WS.send ws $ Cache.toWsMessage request
    answer <- WS.recv ws

    let response = Cache.fromWsMessage answer :: Maybe Cache.Response
    pure response

  case resp of
    Nothing -> pure Nothing
    Just cr -> pure $ deserialize cr

putCache :: ∀ a m. MonadCache m => Serializable a => a -> m Unit
putCache obj = do
  ws <- getCacheConnection
  liftAff $ do
    WS.send ws $ Cache.toWsMessage (serialize Cache.Create obj)
    void $ WS.recv ws

updateCache :: ∀ a m. MonadCache m => Serializable a => a -> m Unit
updateCache obj = do
  ws <- getCacheConnection
  liftAff $ do
    WS.send ws $ Cache.toWsMessage (serialize Cache.Update obj)
    void $ WS.recv ws

