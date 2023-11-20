module Monad.Bench where

import Prelude

import Control.Monad.Reader (class MonadAsk, ReaderT, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, runStateT)
import Data.Array (filter, head)
import Data.Configuration as Config
import Data.Date (month, year)
import Data.DateTime (date, day, hour, millisecond, minute, second, time)
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Messages as Cache
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (padStart)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now (nowDateTime)
import Effect.Random (randomInt)
import Misc.Utils (Radix(..), parseInt)
import Network.WebSocket as WS
import Storage.Database (class Findable, class Persistable, class Updatable)
import Storage.Database as DB
import Storage.Types (Address(..), PhoneNumber(..), User(..))
import Yoga.Postgres as YG

type PadSize = Int

type CacheStats =
  { cacheHit :: Int
  , cacheMiss :: Int
  }

type AppState =
  { users :: CacheStats
  , addresses :: CacheStats
  , phones :: CacheStats
  }

type AppEnvironment =
  { ws :: WS.WebSocket
  , pool :: YG.Pool
  , configuration :: Config.Configuration
  }

-- I could use an RWS but I definetely do not need the W
newtype AppM a = AppM (StateT AppState (ReaderT AppEnvironment Aff) a)

runAppM :: ∀ a. AppM a -> AppState -> AppEnvironment -> Aff (Tuple a AppState)
runAppM (AppM m) is env = runReaderT (runStateT m is) env

-- This Monad can generate requests for the cache starting from an object s
class (MonadAff m) <= MonadCache s m where
  getCache :: WS.WebSocket -> Int -> m (Maybe s)
  putCache :: WS.WebSocket -> s -> m Unit
  updateCache :: WS.WebSocket -> s -> m Unit

-- Commodity logger
class (MonadEffect m) <= MonadLogger m where
  log :: String -> m Unit

type MinSleep = Int
type MaxSleep = Int

-- | This Monad can interact with objects in a PostgreSQL Database
-- | since this is a simulation, the delay for reaching the DB is
-- | configurable
class (MonadAff m, Findable s, Persistable s, Updatable s) <= MonadDatabase s m where
  getDb :: MinSleep -> MaxSleep -> YG.Pool -> Int -> m (Maybe s)
  putDb :: MinSleep -> MaxSleep -> YG.Pool -> s -> m Unit
  updateDb :: MinSleep -> MaxSleep -> YG.Pool -> s -> m Unit

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

padId :: Int -> String
padId = replaceAll (Pattern " ") (Replacement "0") <<< padStart 7 <<< show

getField :: String -> Array Cache.Field -> Maybe Cache.Field
getField fn = filter (_.key >>> ((==) fn)) >>> head

mkUserFields :: User -> Array Cache.Field
mkUserFields (User { firstname, lastname, address, phonenumber }) =
  [ { key: "firstname", value: firstname }
  , { key: "lastname", value: lastname }
  , { key: "address", value: show address }
  , { key: "phonenumber", value: show phonenumber }
  ]

serializeUser :: (Cache.ObjectId -> Array Cache.Field -> Cache.Request) -> User -> Cache.Request
serializeUser c u@(User { id }) =
  let
    objectId = padId id <> "U"
  in
    c objectId (mkUserFields u)

mkAddressFields :: Address -> Array Cache.Field
mkAddressFields (Address { country, city, street, zip }) =
  [ { key: "country", value: country }
  , { key: "city", value: city }
  , { key: "street", value: street }
  , { key: "zip", value: zip }
  ]

serializeAddress :: (Cache.ObjectId -> Array Cache.Field -> Cache.Request) -> Address -> Cache.Request
serializeAddress c a@(Address { id }) =
  let
    objectId = padId id <> "A"
  in
    c objectId (mkAddressFields a)

mkPhoneFields :: PhoneNumber -> Array Cache.Field
mkPhoneFields (PhoneNumber { number, prefix }) =
  [ { key: "number", value: number }
  , { key: "prefix", value: prefix }
  ]

serializePhone :: (Cache.ObjectId -> Array Cache.Field -> Cache.Request) -> PhoneNumber -> Cache.Request
serializePhone c p@(PhoneNumber { id }) =
  let
    objectId = padId id <> "P"
  in
    c objectId (mkPhoneFields p)

type PadLetter = String

getFromCache :: ∀ m. MonadAff m => WS.WebSocket -> Int -> PadLetter -> m (Maybe Cache.Response)
getFromCache ws id pl = do
  let
    paddedId = padId id <> pl
    request = Cache.Get paddedId

  liftAff $ do
    WS.send ws $ Cache.toWsMessage request
    answer <- WS.recv ws

    let response = Cache.fromWsMessage answer :: Maybe Cache.Response
    pure response

-- Simulate a small delay for accessing the DB over the network
instance MonadDatabase PhoneNumber AppM where
  getDb minSleep maxSleep pool id = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    runReaderT (DB.find id) pool

  putDb minSleep maxSleep pool a = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    void $ runReaderT (DB.persist a) pool

  updateDb minSleep maxSleep pool a = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    void $ runReaderT (DB.update a) pool

instance MonadCache PhoneNumber AppM where
  getCache ws id = do
    resp <- getFromCache ws id "P"
    case resp of
      Nothing -> pure Nothing
      Just cr -> pure $ deserializeMsg cr
    where
    deserializeMsg :: Cache.Response -> Maybe PhoneNumber
    deserializeMsg (Cache.GetR objectId fields) = do
      number <- getField "number" fields >>= _.value >>> pure
      prefix <- getField "prefix" fields >>= _.value >>> pure
      cacheId <- parseInt (Radix 10) objectId

      pure $ PhoneNumber { id: cacheId, number, prefix }

    deserializeMsg _ = Nothing

  putCache ws p = liftAff $ do
    WS.send ws $ Cache.toWsMessage (serializePhone Cache.Create p)
    void $ WS.recv ws
  updateCache ws p = liftAff $ do
    WS.send ws $ Cache.toWsMessage (serializePhone Cache.Update p)
    void $ WS.recv ws

instance MonadDatabase Address AppM where
  getDb minSleep maxSleep pool id = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    runReaderT (DB.find id) pool
  putDb minSleep maxSleep pool a = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    void $ runReaderT (DB.persist a) pool
  updateDb minSleep maxSleep pool a = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    void $ runReaderT (DB.update a) pool

instance MonadCache Address AppM where
  getCache ws id = do
    resp <- getFromCache ws id "A"
    case resp of
      Nothing -> pure Nothing
      Just cr -> pure $ deserializeMsg cr

    where
    deserializeMsg :: Cache.Response -> Maybe Address
    deserializeMsg (Cache.GetR objectId fields) = do
      country <- getField "country" fields >>= _.value >>> pure
      city <- getField "city" fields >>= _.value >>> pure
      street <- getField "street" fields >>= _.value >>> pure
      zip <- getField "zip" fields >>= _.value >>> pure
      cacheId <- parseInt (Radix 10) objectId

      pure $ Address { id: cacheId, country, city, street, zip }

    deserializeMsg _ = Nothing

  putCache ws a = liftAff $ do
    WS.send ws $ Cache.toWsMessage (serializeAddress Cache.Create a)
    void $ WS.recv ws

  updateCache ws a = liftAff $ do
    WS.send ws $ Cache.toWsMessage (serializeAddress Cache.Update a)
    void $ WS.recv ws

instance MonadDatabase User AppM where
  getDb minSleep maxSleep pool id = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    runReaderT (DB.find id) pool
  putDb minSleep maxSleep pool a = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    void $ runReaderT (DB.persist a) pool
  updateDb minSleep maxSleep pool a = liftAff $ do
    ri <- liftEffect $ randomInt minSleep maxSleep
    delay (Milliseconds $ toNumber ri)
    void $ runReaderT (DB.update a) pool

instance MonadCache User AppM where
  getCache ws id = do
    resp <- getFromCache ws id "U"
    case resp of
      Nothing -> pure Nothing
      Just cr -> pure $ deserializeMsg cr

    where
    deserializeMsg :: Cache.Response -> Maybe User
    deserializeMsg (Cache.GetR objectId fields) = do
      firstname <- getField "firstname" fields >>= _.value >>> pure
      lastname <- getField "lastname" fields >>= _.value >>> pure
      phonenumber <- getField "phonenumber" fields >>= _.value >>> parseInt (Radix 10)
      address <- getField "address" fields >>= _.value >>> parseInt (Radix 10)

      cacheId <- parseInt (Radix 10) objectId

      pure $ User { id: cacheId, firstname, lastname, phonenumber, address }
    deserializeMsg _ = Nothing

  putCache ws u = liftAff $ do
    WS.send ws $ Cache.toWsMessage (serializeUser Cache.Create u)
    void $ WS.recv ws

  updateCache ws u = liftAff $ do
    WS.send ws $ Cache.toWsMessage (serializeUser Cache.Update u)
    void $ WS.recv ws

instance MonadLogger AppM where
  log x = liftEffect $ prependDateTime x >>= Console.log

    where
    prependDateTime :: String -> Effect String
    prependDateTime s = do
      n <- nowDateTime
      let
        nd = date n
        nt = time n

        ge :: ∀ a. BoundedEnum a => PadSize -> a -> String
        ge ps = replaceAll (Pattern " ") (Replacement "0") <<< padStart ps <<< show <<< fromEnum

        showDate = ge 4 (year nd) <> "-" <> ge 2 (month nd) <> "-" <> ge 2 (day nd)
        showTime = ge 2 (hour nt) <> ":" <> ge 2 (minute nt) <> ":" <> ge 2 (second nt) <> "." <> ge 4 (millisecond nt)

      pure ("[" <> showDate <> " " <> showTime <> "] " <> s)

