module Monad.Bench where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, asks, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Configuration as Config
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Misc.Logger (class MonadLogger, Severity(..))
import Monad.MonadCache (class MonadCache)
import Monad.MonadDatabase (class MonadDatabase)
import Network.WebSocket as WS
import Yoga.Postgres as YG

data Call
  = CacheCall Number Number
  | DatabaseCall Number Number

isCacheCall :: Call -> Boolean
isCacheCall (CacheCall _ _) = true
isCacheCall _ = false

type CacheStats =
  { cacheHit :: Int
  , cacheMiss :: Int
  }

type AppState =
  { users :: CacheStats
  , addresses :: CacheStats
  , phones :: CacheStats
  , calls :: Array Call
  }

type AppEnvironment =
  { ws :: WS.WebSocket
  , pool :: YG.Pool
  , clientId :: String
  , configuration :: Config.Configuration
  }

-- I could use an RWS but I definetely do not need the W
newtype AppM a = AppM (StateT AppState (ReaderT AppEnvironment Aff) a)

runAppM :: ∀ a. AppM a -> AppState -> AppEnvironment -> Aff (Tuple a AppState)
runAppM (AppM m) is env = runReaderT (runStateT m is) env

instance MonadLogger AppM where
  getContext = pure "Bench"
  getLogLevel = pure Info

instance MonadCache AppM where
  getCacheConnection = asks _.ws

instance MonadDatabase AppM where
  getDatabaseConnection = asks _.pool

derive newtype instance Functor AppM
derive newtype instance Applicative AppM
derive newtype instance Apply AppM
derive newtype instance Bind AppM
derive newtype instance Monad AppM

derive newtype instance MonadRec AppM
derive newtype instance MonadEffect AppM
derive newtype instance MonadAff AppM
derive newtype instance MonadState AppState AppM
derive newtype instance MonadAsk AppEnvironment AppM

