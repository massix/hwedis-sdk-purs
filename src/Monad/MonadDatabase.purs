module Monad.MonadDatabase
  ( class MonadDatabase
  , getDatabaseConnection
  , getDb
  , updateDb
  , putDb
  ) where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Argonaut (class DecodeJson)
import Data.Int (toNumber)
import Data.Maybe (Maybe)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Random (randomInt)
import Storage.Database (class Findable, class Persistable, class Updatable)
import Storage.Database as DB
import Yoga.Postgres as YG

type MinSleep = Int
type MaxSleep = Int

class (MonadAff m) <= MonadDatabase m where
  getDatabaseConnection :: m YG.Pool

random :: ∀ m. MonadEffect m => Int -> Int -> m Int
random min max = liftEffect $ randomInt min max

sleepFor :: ∀ m. MonadAff m => Int -> m Unit
sleepFor = liftAff <<< delay <<< Milliseconds <<< toNumber

getDb :: ∀ a m. MonadDatabase m => Findable a => DecodeJson a => MinSleep -> MaxSleep -> Int -> m (Maybe a)
getDb minSleep maxSleep id = do
  pool <- getDatabaseConnection
  liftAff $ do
    ri <- random minSleep maxSleep
    sleepFor ri
    runReaderT (DB.find id) pool

updateDb :: ∀ a m. MonadDatabase m => Updatable a => DecodeJson a => MinSleep -> MaxSleep -> a -> m Unit
updateDb minSleep maxSleep id = do
  pool <- getDatabaseConnection
  liftAff $ do
    ri <- random minSleep maxSleep
    sleepFor ri
    void $ runReaderT (DB.update id) pool

putDb :: ∀ a m. MonadDatabase m => Persistable a => DecodeJson a => MinSleep -> MaxSleep -> a -> m Unit
putDb minSleep maxSleep id = do
  pool <- getDatabaseConnection
  liftAff $ do
    ri <- random minSleep maxSleep
    sleepFor ri
    void $ runReaderT (DB.persist id) pool

