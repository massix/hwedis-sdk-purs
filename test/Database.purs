module Test.Database where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (length, (..))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Storage.Database as DB
import Storage.Types (Address(..), PhoneNumber(..), User(..), UserWithJoin(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldSatisfy)
import Test.TestContainers (WaitStrategy(..), containerHost, containerPort, exposePort, mkGenericContainer, setEnvironment, start, stop, waitStrategy)
import Unsafe.Coerce (unsafeCoerce)
import Yoga.Postgres (Query(..), connect, end, mkPool, query, release) as YG
import Yoga.Postgres as PG
import Yoga.Postgres.SqlValue (toSql) as YG

convert :: ∀ t. DecodeJson t => Foreign -> Either Error t
convert =
  let
    toJson :: Foreign -> Json
    toJson = unsafeCoerce

    toError :: JsonDecodeError -> Error
    toError = printJsonDecodeError >>> error
  in
    toJson >>> decodeJson >>> lmap toError

withDatabase :: ∀ a. (PG.ConnectionString -> Aff a) -> Aff a
withDatabase func = do
  container <- liftEffect $
    mkGenericContainer "postgres:14-alpine"
      >>= flip exposePort 5432
      >>= flip waitStrategy (LogWaitStrategy "database system is ready to accept connections" (Just 2))
      >>= flip setEnvironment
        [ { key: "POSTGRES_USER", value: "test" }
        , { key: "POSTGRES_PASSWORD", value: "test" }
        , { key: "POSTGRES_DB", value: "test" }
        ]

  started <- start container
  host <- liftEffect $ containerHost started
  port <- liftEffect $ containerPort started 5432
  cs <- connectionString host port
  res <- func cs

  void $ stop started
  pure res

  where
  connectionString :: Maybe String -> Maybe Int -> Aff String
  connectionString (Just h) (Just p) = pure $ "postgres://test:test@" <> h <> ":" <> show p <> "/test"
  connectionString _ _ = throwError $ error "Could not detect container information"

testDatabase :: Spec Unit
testDatabase = do
  describe "Database" do
    it "should create tables if they do not exist and not fail if they already exist" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs

        -- Here we create the tables
        traverse_ (\_ -> DB.runDatabaseT DB.createTables connectionInfo) [ 1 .. 1000 ]

        -- Make sure that the tables exist
        pool <- liftEffect $ YG.mkPool connectionInfo
        client <- YG.connect pool

        let
          q :: YG.Query { table_name :: String }
          q = YG.Query "select table_name from information_schema.tables where table_schema = $1"

        result <- YG.query convert q [ YG.toSql "public" ] client

        length result `shouldEqual` 3
        result `shouldContain` { table_name: "address" }
        result `shouldContain` { table_name: "phone" }
        result `shouldContain` { table_name: "user" }

        liftEffect $ YG.release client
        liftEffect $ YG.end pool

    it "should create phone numbers" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        res <- DB.runDatabaseT (DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "123456789" }) connectionInfo
        res `shouldSatisfy` isJust

        let PhoneNumber { prefix, number } = unsafePartial $ fromJust res
        prefix `shouldEqual` "+33"
        number `shouldEqual` "123456789"

    it "should create addresses" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        res <- DB.runDatabaseT (DB.persist $ Address { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo
        res `shouldSatisfy` isJust

        let Address { country, street, city, zip } = unsafePartial $ fromJust res
        country `shouldEqual` "France"
        street `shouldEqual` "Street"
        city `shouldEqual` "City"
        zip `shouldEqual` "1234"

    it "should create users" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        address <- DB.runDatabaseT (DB.persist $ Address { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo
        address `shouldSatisfy` isJust
        let (Address { id: addrId }) = unsafePartial $ fromJust address

        phone <- DB.runDatabaseT (DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "123456789" }) connectionInfo
        phone `shouldSatisfy` isJust
        let (PhoneNumber { id: phoneId }) = unsafePartial $ fromJust phone

        user <- DB.runDatabaseT (DB.persist $ User { id: 0, firstname: "John", lastname: "Doe", address: addrId, phonenumber: phoneId }) connectionInfo
        user `shouldSatisfy` isJust
        let (User { firstname, lastname, address, phonenumber }) = unsafePartial $ fromJust user

        firstname `shouldEqual` "John"
        lastname `shouldEqual` "Doe"
        address `shouldEqual` addrId
        phonenumber `shouldEqual` phoneId

    it "should retrieve an address with their id" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo
        void $ DB.runDatabaseT (DB.persist $ Address { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo

        address <- DB.runDatabaseT (DB.find 1) connectionInfo
        address `shouldSatisfy` isJust
        let Address { id, country, street, city, zip } = unsafePartial $ fromJust address

        id `shouldEqual` 1
        country `shouldEqual` "France"
        street `shouldEqual` "Street"
        city `shouldEqual` "City"
        zip `shouldEqual` "1234"

    it "should retrieve a phone with their id" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo
        void $ DB.runDatabaseT (DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "123456789" }) connectionInfo

        phone <- DB.runDatabaseT (DB.find 1) connectionInfo
        phone `shouldSatisfy` isJust

        let PhoneNumber { id, prefix, number } = unsafePartial $ fromJust phone

        id `shouldEqual` 1
        prefix `shouldEqual` "+33"
        number `shouldEqual` "123456789"

    it "should retrieve an user with their id" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        address <- DB.runDatabaseT (DB.persist $ Address { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo
        address `shouldSatisfy` isJust
        let Address { id: addrId } = unsafePartial $ fromJust address

        phone <- DB.runDatabaseT (DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "123456789" }) connectionInfo
        phone `shouldSatisfy` isJust
        let PhoneNumber { id: phoneId } = unsafePartial $ fromJust phone

        user <- DB.runDatabaseT (DB.persist $ User { id: 0, firstname: "John", lastname: "Doe", address: addrId, phonenumber: phoneId }) connectionInfo
        let User { id: userId } = unsafePartial $ fromJust user

        byId <- DB.runDatabaseT (DB.find userId) connectionInfo
        byId `shouldSatisfy` isJust
        let (User { firstname, lastname, address, phonenumber }) = unsafePartial $ fromJust byId

        firstname `shouldEqual` "John"
        lastname `shouldEqual` "Doe"
        address `shouldEqual` 1
        phonenumber `shouldEqual` 1

    it "should persist an user with join" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        res <- DB.runDatabaseT
          ( DB.persistUserWithJoin $ UserWithJoin
              { id: 0
              , firstname: "newUser"
              , lastname: "lastName"
              , address: Address
                  { id: 0
                  , country: "France"
                  , street: "SomeStreet"
                  , city: "Paris"
                  , zip: "92000"
                  }
              , phonenumber: PhoneNumber
                  { id: 0
                  , prefix: "+33"
                  , number: "12345"
                  }
              }
          )
          connectionInfo

        res `shouldSatisfy` isJust
        let UserWithJoin { id, address: Address { id: adid, country }, phonenumber: PhoneNumber { id: phid, prefix } } = unsafePartial $ fromJust res

        id `shouldEqual` 1
        adid `shouldEqual` 1
        country `shouldEqual` "France"
        phid `shouldEqual` 1
        prefix `shouldEqual` "+33"

    it "should retrieve an user with join" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo
        void $ DB.runDatabaseT
          ( DB.persistUserWithJoin $ UserWithJoin
              { id: 0
              , firstname: "newUser"
              , lastname: "lastName"
              , address: Address
                  { id: 0
                  , country: "France"
                  , street: "SomeStreet"
                  , city: "Paris"
                  , zip: "92000"
                  }
              , phonenumber: PhoneNumber
                  { id: 0
                  , prefix: "+33"
                  , number: "12345"
                  }
              }
          )
          connectionInfo

        withJoin <- DB.runDatabaseT (DB.findUserWithJoin 1) connectionInfo
        withJoin `shouldSatisfy` isJust

        let UserWithJoin { firstname, address: Address { country }, phonenumber: PhoneNumber { number } } = unsafePartial $ fromJust withJoin
        firstname `shouldEqual` "newUser"
        country `shouldEqual` "France"
        number `shouldEqual` "12345"

    it "should count users" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT
          ( do
              DB.createTables
              void $ DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "123456789" }
              void $ DB.persist $ Address { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }
          )
          connectionInfo

        firstCount <- DB.runDatabaseT DB.countUsers connectionInfo
        traverse_ (\u -> DB.runDatabaseT (DB.persist u) connectionInfo)
          [ User
              { id: 0
              , firstname: "John"
              , lastname: "Doe"
              , address: 1
              , phonenumber: 1
              }
          , User
              { id: 0
              , firstname: "Jane"
              , lastname: "Doe"
              , address: 1
              , phonenumber: 1
              }
          ]

        secondCount <- DB.runDatabaseT DB.countUsers connectionInfo
        firstCount `shouldEqual` 0
        secondCount `shouldEqual` 2

    it "should count addresses" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs

        ci <- DB.runDatabaseT
          ( do
              DB.createTables
              initialCount <- DB.countAddresses
              void $ DB.persist $ Address { id: 0, country: "c0", street: "s0", city: "c0", zip: "z0" }
              void $ DB.persist $ Address { id: 0, country: "c1", street: "s1", city: "c1", zip: "z1" }
              pure initialCount
          )
          connectionInfo

        secondCount <- DB.runDatabaseT DB.countAddresses connectionInfo
        ci `shouldEqual` 0
        secondCount `shouldEqual` 2

    it "should count phonenumbers" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs

        ci <- DB.runDatabaseT
          ( do
              DB.createTables
              initialCount <- DB.countPhoneNumbers
              void $ DB.persist $ PhoneNumber { id: 0, number: "1234", prefix: "+33" }
              void $ DB.persist $ PhoneNumber { id: 0, number: "1235", prefix: "+34" }
              pure initialCount
          )
          connectionInfo

        secondCount <- DB.runDatabaseT DB.countPhoneNumbers connectionInfo
        ci `shouldEqual` 0
        secondCount `shouldEqual` 2

    it "should update address" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        addr <- DB.runDatabaseT (DB.persist $ Address { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo
        let Address { id: oldId, city: oldCity, zip: oldZip } = unsafePartial $ fromJust addr

        result <- DB.runDatabaseT (DB.update $ Address { id: oldId, country: "Italy", street: "Different", city: oldCity, zip: oldZip }) connectionInfo
        result `shouldSatisfy` isJust

        let Address { country, street, zip } = unsafePartial $ fromJust result
        country `shouldEqual` "Italy"
        street `shouldEqual` "Different"
        zip `shouldEqual` "1234"

    it "should update phone" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        phone <- DB.runDatabaseT (DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "123456789" }) connectionInfo
        let PhoneNumber { id: oldId } = unsafePartial $ fromJust phone

        result <- DB.runDatabaseT (DB.update $ PhoneNumber { id: oldId, prefix: "+44", number: "987654321" }) connectionInfo
        result `shouldSatisfy` isJust

        let PhoneNumber { prefix, number } = unsafePartial $ fromJust result
        prefix `shouldEqual` "+44"
        number `shouldEqual` "987654321"

    it "should update user" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        currentUser <- DB.runDatabaseT
          ( do
              DB.createTables
              void $ DB.persist $ Address { id: 0, country: "France", city: "Valbonne", street: "SomeStreet", zip: "06560" }
              void $ DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "0612345678" }
              DB.persist $ User { id: 0, firstname: "John", lastname: "Doe", address: 1, phonenumber: 1 }
          )
          connectionInfo

        let (User { id: uid, address, phonenumber }) = unsafePartial $ fromJust currentUser
        uid `shouldEqual` 1

        modified <- DB.runDatabaseT (DB.update $ User { id: uid, firstname: "Jim", lastname: "Big", address, phonenumber }) connectionInfo
        modified `shouldSatisfy` isJust

        let (User { id: newId, firstname, lastname }) = unsafePartial $ fromJust modified
        firstname `shouldEqual` "Jim"
        lastname `shouldEqual` "Big"
        newId `shouldEqual` uid

    it "should delete user" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        currentUser <- DB.runDatabaseT
          ( do
              DB.createTables
              void $ DB.persist $ Address { id: 0, country: "France", city: "Valbonne", street: "SomeStreet", zip: "06560" }
              void $ DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "0612345678" }
              DB.persist $ User { id: 0, firstname: "John", lastname: "Doe", address: 1, phonenumber: 1 }
          )
          connectionInfo

        currentUser `shouldSatisfy` isJust
        res <- DB.runDatabaseT (DB.delete (unsafePartial $ fromJust currentUser)) connectionInfo
        res `shouldSatisfy` isJust

        let (User { id, firstname, lastname }) = unsafePartial $ fromJust currentUser
        id `shouldEqual` 1
        firstname `shouldEqual` "John"
        lastname `shouldEqual` "Doe"

    it "should delete address" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        currentAddress <- DB.runDatabaseT
          ( do
              DB.createTables
              DB.persist $ Address { id: 0, country: "France", city: "Valbonne", street: "SomeStreet", zip: "06560" }
          )
          connectionInfo

        currentAddress `shouldSatisfy` isJust
        res <- DB.runDatabaseT (DB.delete (unsafePartial $ fromJust currentAddress)) connectionInfo
        res `shouldSatisfy` isJust

        let (Address { id, country, city, street }) = unsafePartial $ fromJust currentAddress
        id `shouldEqual` 1
        country `shouldEqual` "France"
        city `shouldEqual` "Valbonne"
        street `shouldEqual` "SomeStreet"

    it "should delete phone" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        currentPhone <- DB.runDatabaseT
          ( do
              DB.createTables
              DB.persist $ PhoneNumber { id: 0, prefix: "+33", number: "0612345678" }
          )
          connectionInfo

        currentPhone `shouldSatisfy` isJust
        res <- DB.runDatabaseT (DB.delete (unsafePartial $ fromJust currentPhone)) connectionInfo
        res `shouldSatisfy` isJust

        let (PhoneNumber { id, prefix, number }) = unsafePartial $ fromJust currentPhone
        id `shouldEqual` 1
        prefix `shouldEqual` "+33"
        number `shouldEqual` "0612345678"
