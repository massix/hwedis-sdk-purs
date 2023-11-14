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
  , persistUserWithJoin
  , countUsers
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Argonaut (class DecodeJson, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int (round)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (isNaN)
import Effect (Effect)
import Effect.Aff (Aff, Error, bracket, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Node.Process as NP
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Postgres (Query(..), connect, execute_, query, queryOne_, release) as YG
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
  liftAff $ withPool pool $ \c -> YG.query fromDbResult q [ YG.toSql p.prefix, YG.toSql p.number ] c >>= pure <<< head
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

persistUserWithJoin :: UserWithJoin -> DatabaseT (Maybe UserWithJoin)
persistUserWithJoin u = do
  -- persist other data first
  addr <- persistAddress u.address
  phone <- persistPhone u.phonenumber

  case (generateUser u addr phone) of
    Nothing -> pure Nothing
    Just x -> do
      user <- persistUser x
      case user of
        Nothing -> pure Nothing
        Just finalUser -> findUserById finalUser.id

  where
  generateUser :: UserWithJoin -> Maybe Address -> Maybe PhoneNumber -> Maybe User
  generateUser user addr ph = do
    a <- addr
    p <- ph
    pure { id: user.id, firstname: user.firstname, lastname: user.lastname, phonenumber: p.id, address: a.id }

findAddressById :: Int -> DatabaseT (Maybe Address)
findAddressById addressId = do
  pool <- ask
  liftAff $ withPool pool $ \c -> YG.query fromDbResult q [ YG.toSql addressId ] c >>= pure <<< head

  where
  q :: YG.Query Address
  q = YG.Query "select * from address where id = $1"

findPhoneById :: Int -> DatabaseT (Maybe PhoneNumber)
findPhoneById phoneId = do
  pool <- ask
  liftAff $ withPool pool $ \c -> YG.query fromDbResult q [ YG.toSql phoneId ] c >>= pure <<< head

  where
  q :: YG.Query PhoneNumber
  q = YG.Query "select * from phone where id = $1"

findUserById :: Int -> DatabaseT (Maybe UserWithJoin)
findUserById userId = do
  pool <- ask
  user <- liftAff $ withPool pool $ \c -> YG.query fromDbResult q [ YG.toSql userId ] c >>= pure <<< head

  case user of
    Nothing -> pure Nothing
    Just u -> do
      phone <- findPhoneById u.phonenumber
      address <- findAddressById u.address
      pure $ buildUser u phone address

  where
  q :: YG.Query User
  q = YG.Query "select * from \"user\" where id = $1"

  buildUser :: User -> Maybe PhoneNumber -> Maybe Address -> Maybe UserWithJoin
  buildUser u p a = do
    ph <- p
    ad <- a
    pure { id: u.id, firstname: u.firstname, lastname: u.lastname, phonenumber: ph, address: ad }

foreign import parseIntImpl :: String -> Int -> Number

newtype Radix = Radix Int

parseInt :: String -> Radix -> Maybe Int
parseInt n (Radix r) =
  let
    result = parseIntImpl n r
  in
    if r >= 2 && r <= 36 then
      if isNaN result then
        Nothing
      else
        Just $ round result
    else
      Nothing

countUsers :: DatabaseT Int
countUsers = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    res <- YG.queryOne_ fromDbResult q c
    let result = res >>= _.count >>> flip parseInt (Radix 10)
    case result of
      Nothing -> pure 0
      Just x -> pure x

  where
  q :: YG.Query { count :: String }
  q = YG.Query "select count(*) as count from \"user\""

fromDbResult :: ∀ a. DecodeJson a => Foreign -> Either Error a
fromDbResult = unsafeCoerce >>> decodeJson >>> lmap toError
  where
  toError :: JsonDecodeError -> Error
  toError = printJsonDecodeError >>> error

withPool :: ∀ a. PG.Pool -> (PG.Client -> Aff a) -> Aff a
withPool pool func = bracket (YG.connect pool) (liftEffect <<< YG.release) func

