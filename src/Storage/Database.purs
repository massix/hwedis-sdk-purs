module Storage.Database
  ( class Persistable
  , class Updatable
  , class Findable
  , class Deletable
  , createQuery
  , updateQuery
  , deleteQuery
  , getId
  , findQuery
  , DatabaseT
  , runDatabaseT
  , createTables
  , persist
  , persistUserWithJoin
  , find
  , update
  , delete
  , deleteWhereId
  , countUsers
  , countAddresses
  , countPhoneNumbers
  , findUserWithJoin
  , cleanDatabase
  , dropTables
  ) where

import Prelude

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Argonaut (class DecodeJson, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (head)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Error, bracket, error)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Misc.Utils (Radix(..), parseInt)
import Storage.Types (Address(..), PhoneNumber(..), User(..), UserWithJoin(..))
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Postgres (Query(..), connect, execute_, query, queryOne_, release) as YG
import Yoga.Postgres as PG
import Yoga.Postgres.SqlValue (SqlValue, toSql) as YG

type DatabaseT a = ReaderT PG.Pool Aff a

type YGQuery :: Type -> Type
type YGQuery a = { query :: YG.Query a, params :: Array YG.SqlValue }

class Persistable t where
  createQuery :: t -> YGQuery t

class Findable t where
  findQuery :: Int -> YGQuery t
  getId :: t -> Int

class Findable t <= Updatable t where
  updateQuery :: t -> YGQuery t

class Deletable t where
  deleteQuery :: t -> YGQuery t

instance Persistable User where
  createQuery (User { firstname, lastname, phonenumber, address }) =
    { query: YG.Query "insert into \"user\" (firstname, lastname, phonenumber, address) values ($1, $2, $3, $4) returning *"
    , params: [ YG.toSql firstname, YG.toSql lastname, YG.toSql phonenumber, YG.toSql address ]
    }

instance Findable User where
  findQuery id =
    { query: YG.Query "select * from \"user\" where id = $1"
    , params: [ YG.toSql id ]
    }
  getId (User { id }) = id

instance Updatable User where
  updateQuery (User { id, firstname, lastname, phonenumber, address }) =
    { query: YG.Query "update \"user\" set firstname = $1, lastname = $2, phonenumber = $3, address = $4 where id = $5 returning *"
    , params: [ YG.toSql firstname, YG.toSql lastname, YG.toSql phonenumber, YG.toSql address, YG.toSql id ]
    }

instance Deletable User where
  deleteQuery (User { id }) =
    { query: YG.Query "delete from \"user\" where id = $1 returning *"
    , params: [ YG.toSql id ]
    }

instance Persistable Address where
  createQuery (Address { country, city, street, zip }) =
    { query: YG.Query "insert into address (country, city, street, zip) values ($1, $2, $3, $4) returning *"
    , params: [ YG.toSql country, YG.toSql city, YG.toSql street, YG.toSql zip ]
    }

instance Findable Address where
  findQuery id =
    { query: YG.Query "select * from address where id = $1"
    , params: [ YG.toSql id ]
    }
  getId (Address { id }) = id

instance Updatable Address where
  updateQuery (Address { id, country, city, street, zip }) =
    { query: YG.Query "update address set country = $1, city = $2, street = $3, zip = $4 where id = $5 returning *"
    , params: [ YG.toSql country, YG.toSql city, YG.toSql street, YG.toSql zip, YG.toSql id ]
    }

instance Deletable Address where
  deleteQuery (Address { id }) =
    { query: YG.Query "delete from address where id = $1 returning *"
    , params: [ YG.toSql id ]
    }

instance Persistable PhoneNumber where
  createQuery (PhoneNumber { number, prefix }) =
    { query: YG.Query "insert into phone (number, prefix) values ($1, $2) returning *"
    , params: [ YG.toSql number, YG.toSql prefix ]
    }

instance Findable PhoneNumber where
  findQuery id =
    { query: YG.Query "select * from phone where id = $1"
    , params: [ YG.toSql id ]
    }
  getId (PhoneNumber { id }) = id

instance Updatable PhoneNumber where
  updateQuery (PhoneNumber { id, prefix, number }) =
    { query: YG.Query "update phone set prefix = $1, number = $2 where id = $3 returning *"
    , params: [ YG.toSql prefix, YG.toSql number, YG.toSql id ]
    }

instance Deletable PhoneNumber where
  deleteQuery (PhoneNumber { id }) =
    { query: YG.Query "delete from phone where id = $1 returning *"
    , params: [ YG.toSql id ]
    }

runDatabaseT :: ∀ a. DatabaseT a -> PG.ConnectionInfo -> Aff a
runDatabaseT m ci = bracket (liftEffect $ PG.mkPool ci) (PG.end >>> liftEffect) (runReaderT m)

extractHead :: ∀ a m. Monad m => Array a -> m (Maybe a)
extractHead = pure <<< head

createTables :: DatabaseT Unit
createTables = do
  pool <- ask

  liftAff
    $ withPool pool
    $ \c -> do
        YG.execute_ (YG.Query "create table if not exists address (id serial primary key, country text, city text, street text, zip text)") c
        YG.execute_ (YG.Query "create table if not exists phone (id serial primary key, prefix text, number text)") c
        YG.execute_ (YG.Query "create table if not exists \"user\" (id serial primary key, firstname text, lastname text, phonenumber int references phone(id), address int references address(id))") c

persist :: ∀ t. Persistable t => DecodeJson t => t -> DatabaseT (Maybe t)
persist t = do
  pool <- ask
  let q = createQuery t
  liftAff $ withPool pool $ \c ->
    YG.query fromDbResult q.query q.params c >>= extractHead

find :: ∀ t. Findable t => DecodeJson t => Int -> DatabaseT (Maybe t)
find id = do
  pool <- ask
  let q = findQuery id
  liftAff $ withPool pool $ \c -> YG.query fromDbResult q.query q.params c >>= extractHead

update :: ∀ t. Updatable t => DecodeJson t => t -> DatabaseT (Maybe t)
update t = do
  pool <- ask
  existing <- find_ t
  let q = updateQuery t

  liftAff $ withPool pool $ \c -> case existing of
    Nothing -> pure Nothing
    Just _ -> YG.query fromDbResult q.query q.params c >>= extractHead

  where
  find_ :: t -> DatabaseT (Maybe t)
  find_ = find <<< getId

delete :: ∀ t. Deletable t => DecodeJson t => t -> DatabaseT (Maybe t)
delete t = do
  pool <- ask
  let q = deleteQuery t
  liftAff $ withPool pool $ \c -> YG.query fromDbResult q.query q.params c >>= extractHead

deleteWhereId :: ∀ t. Deletable t => Findable t => DecodeJson t => Int -> DatabaseT (Maybe t)
deleteWhereId id = find id >>= case _ of
  Nothing -> pure Nothing
  Just x -> delete x

persistUserWithJoin :: UserWithJoin -> DatabaseT (Maybe UserWithJoin)
persistUserWithJoin u@(UserWithJoin { address, phonenumber }) = do
  -- persist other data first
  addr <- persist address
  phone <- persist phonenumber

  case (generateUser u addr phone) of
    Nothing -> pure Nothing
    Just x -> do
      user <- persist x
      case user of
        Nothing -> pure Nothing
        Just (User { id }) -> findUserWithJoin id

  where
  generateUser :: UserWithJoin -> Maybe Address -> Maybe PhoneNumber -> Maybe User
  generateUser (UserWithJoin { id, firstname, lastname }) addr ph = do
    (Address { id: addressId }) <- addr
    (PhoneNumber { id: phoneId }) <- ph
    pure $ User { id, firstname, lastname, phonenumber: phoneId, address: addressId }

findUserWithJoin :: Int -> DatabaseT (Maybe UserWithJoin)
findUserWithJoin userId = do
  user <- find userId

  case user of
    Nothing -> pure Nothing
    Just u@(User { phonenumber, address }) -> do
      phone <- find phonenumber
      address' <- find address
      pure $ buildUser u phone address'

  where
  buildUser :: User -> Maybe PhoneNumber -> Maybe Address -> Maybe UserWithJoin
  buildUser (User { id, firstname, lastname }) p a = do
    ph <- p
    ad <- a
    pure $ UserWithJoin { id, firstname, lastname, phonenumber: ph, address: ad }

count_ :: String -> DatabaseT Int
count_ table = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    res <- YG.queryOne_ fromDbResult q c
    let result = res >>= parseInt (Radix 10) <<< _.count
    case result of
      Nothing -> pure 0
      Just x -> pure x

  where
  q :: YG.Query { count :: String }
  q = YG.Query $ "select count(*) as count from " <> table


countUsers :: DatabaseT Int
countUsers = count_ "\"user\""

countAddresses :: DatabaseT Int
countAddresses = count_ "address"

countPhoneNumbers :: DatabaseT Int
countPhoneNumbers = count_ "phone"

cleanDatabase :: DatabaseT Unit
cleanDatabase = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    YG.execute_ (YG.Query "truncate table \"user\", phone, address") c

dropTables :: DatabaseT Unit
dropTables = do
  pool <- ask
  liftAff $ withPool pool $ \c -> do
    YG.execute_ (YG.Query "drop table if exists \"user\", phone, address") c

fromDbResult :: ∀ a. DecodeJson a => Foreign -> Either Error a
fromDbResult = unsafeCoerce >>> decodeJson >>> lmap toError
  where
  toError :: JsonDecodeError -> Error
  toError = printJsonDecodeError >>> error

withPool :: ∀ a. PG.Pool -> (PG.Client -> Aff a) -> Aff a
withPool pool func = bracket (YG.connect pool) (liftEffect <<< YG.release) func

