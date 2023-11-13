module Storage.Database
  ( User
  , UserWithJoin
  , PhoneNumber
  , Address
  , DatabaseT
  , runDatabaseT
  , configureFromEnvironment
  , createTables
  , persistPhone
  , persistAddress
  , persistUser
  , findAddressById
  , findPhoneById
  , findUserById
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Argonaut (class DecodeJson, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, Error, bracket, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Node.Process as NP
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Postgres (Query(..), connect, execute_, query, release) as YG
import Yoga.Postgres as PG
import Yoga.Postgres.SqlValue (toSql) as YG

type DatabaseT a = ReaderT PG.Pool Aff a

-- This is our "model"
type User =
  { id :: Int
  , firstname :: String
  , lastname :: String
  , phonenumber :: Int -- ^ Foreign Key
  , address :: Int -- ^ Foreign Key
  }

type UserWithJoin =
  { id :: Int
  , firstname :: String
  , lastname :: String
  , phonenumber :: PhoneNumber -- ^ Foreign Key
  , address :: Address -- ^ Foreign Key
  }

type PhoneNumber =
  { id :: Int
  , prefix :: String
  , number :: String
  }

type Address =
  { id :: Int
  , country :: String
  , city :: String
  , street :: String
  , zip :: String
  }

configureFromEnvironment :: Effect String
configureFromEnvironment = do
  host <- NP.lookupEnv "PG_HOST"
  port <- NP.lookupEnv "PG_PORT"
  user <- NP.lookupEnv "PG_USER"
  pass <- NP.lookupEnv "PG_PASS"
  db <- NP.lookupEnv "PG_DATABASE"

  pure $ "postgres://"
    <> fromMaybe "local" user
    <> ":"
    <> fromMaybe "local" pass
    <> "@"
    <> fromMaybe "localhost" host
    <> ":"
    <> fromMaybe "5432" port
    <> "/"
    <> fromMaybe "test" db

runDatabaseT :: ∀ a. DatabaseT a -> PG.ConnectionInfo -> Aff a
runDatabaseT m ci = bracket (liftEffect $ PG.mkPool ci) (PG.end >>> liftEffect) (runReaderT m)

createTables :: DatabaseT Unit
createTables = do
  pool <- ask

  liftAff
    $ withPool pool
    $ \c -> do
        YG.execute_ (YG.Query "create table if not exists address (id serial primary key, country text, city text, street text, zip text)") c
        YG.execute_ (YG.Query "create table if not exists phone (id serial primary key, prefix text, number text)") c
        YG.execute_ (YG.Query "create table if not exists \"user\" (id serial primary key, firstname text, lastname text, phonenumber int references phone(id), address int references address(id))") c

persistPhone :: PhoneNumber -> DatabaseT (Maybe PhoneNumber)
persistPhone p = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    res <- YG.query fromDbResult q [ YG.toSql p.prefix, YG.toSql p.number ] c
    pure $ head res
  where
  q :: YG.Query PhoneNumber
  q = YG.Query "insert into phone (prefix, number) values ($1, $2) returning *"

persistAddress :: Address -> DatabaseT (Maybe Address)
persistAddress a = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    res <- YG.query fromDbResult q [ YG.toSql a.country, YG.toSql a.city, YG.toSql a.street, YG.toSql a.zip ] c
    pure $ head res
  where
  q :: YG.Query Address
  q = YG.Query "insert into address (country, city, street, zip) values ($1, $2, $3, $4) returning *"

persistUser :: User -> DatabaseT (Maybe User)
persistUser u = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    res <- YG.query fromDbResult q [ YG.toSql u.firstname, YG.toSql u.lastname, YG.toSql u.phonenumber, YG.toSql u.address ] c
    pure $ head res
  where
  q :: YG.Query User
  q = YG.Query "insert into \"user\" (firstname, lastname, phonenumber, address) values ($1, $2, $3, $4) returning *"

findAddressById :: Int -> DatabaseT (Maybe Address)
findAddressById addressId = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    res <- YG.query fromDbResult q [ YG.toSql addressId ] c
    pure $ head res
  where
  q :: YG.Query Address
  q = YG.Query "select * from address where id = $1"

findPhoneById :: Int -> DatabaseT (Maybe PhoneNumber)
findPhoneById phoneId = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    res <- YG.query fromDbResult q [ YG.toSql phoneId ] c
    pure $ head res
  where
  q :: YG.Query PhoneNumber
  q = YG.Query "select * from phone where id = $1"

findUserById :: Int -> DatabaseT (Maybe UserWithJoin)
findUserById userId = do
  pool <- ask
  user <- liftAff $ withPool pool $ \c -> do
    res <- YG.query fromDbResult q [ YG.toSql userId ] c
    let res' = head res
    case res' of
      Nothing -> pure Nothing
      Just x -> pure $ Just x

  case user of
    Nothing -> pure Nothing
    Just u -> do
      phone <- findPhoneById u.phonenumber
      address <- findAddressById u.address
      case phone of
        Nothing -> pure Nothing
        Just p -> case address of
          Nothing -> pure Nothing
          Just a -> pure $ Just { id: u.id, firstname: u.firstname, lastname: u.lastname, phonenumber: p, address: a }

  where
  q :: YG.Query User
  q = YG.Query "select * from \"user\" where id = $1"

fromDbResult :: ∀ a. DecodeJson a => Foreign -> Either Error a
fromDbResult = unsafeCoerce >>> decodeJson >>> lmap toError
  where
  toError :: JsonDecodeError -> Error
  toError = printJsonDecodeError >>> error

withPool :: ∀ a. PG.Pool -> (PG.Client -> Aff a) -> Aff a
withPool pool func = bracket (YG.connect pool) (liftEffect <<< YG.release) func

