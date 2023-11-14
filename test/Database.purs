module Test.Database where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array (length)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Effect.Aff (Aff, Error, error, throwError)
import Effect.Class (liftEffect)
import Foreign (Foreign)
import Partial.Unsafe (unsafePartial)
import Storage.Database as DB
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual, shouldNotEqual, shouldSatisfy)
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
    it "should create tables if they do not exist" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs

        -- Here we create the tables
        DB.runDatabaseT DB.createTables connectionInfo

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

        res <- DB.runDatabaseT (DB.persistPhone { id: 0, prefix: "+33", number: "123456789" }) connectionInfo
        let fmRes = fromMaybe { id: 0, prefix: "", number: "" } res

        res `shouldSatisfy` isJust
        fmRes `shouldNotEqual` { id: 0, prefix: "", number: "" }
        fmRes.prefix `shouldEqual` "+33"
        fmRes.number `shouldEqual` "123456789"

    it "should create addresses" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        res <- DB.runDatabaseT (DB.persistAddress { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo
        let fmRes = fromMaybe { id: 0, country: "", street: "", city: "", zip: "" } res

        res `shouldSatisfy` isJust
        fmRes `shouldNotEqual` { id: 0, country: "", street: "", city: "", zip: "" }
        fmRes.country `shouldEqual` "France"
        fmRes.street `shouldEqual` "Street"
        fmRes.city `shouldEqual` "City"
        fmRes.zip `shouldEqual` "1234"

    it "should create users" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        address <- DB.runDatabaseT (DB.persistAddress { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo
        let address' = fromMaybe { id: 0, country: "", street: "", city: "", zip: "" } address
        phone <- DB.runDatabaseT (DB.persistPhone { id: 0, prefix: "+33", number: "123456789" }) connectionInfo
        let phone' = fromMaybe { id: 0, prefix: "", number: "" } phone
        user <- DB.runDatabaseT (DB.persistUser { id: 0, firstname: "John", lastname: "Doe", address: (_.id address'), phonenumber: (_.id phone') }) connectionInfo
        let user' = fromMaybe { id: 0, firstname: "", lastname: "", address: 0, phonenumber: 0 } user

        user `shouldSatisfy` isJust
        user' `shouldNotEqual` { id: 0, firstname: "", lastname: "", address: 0, phonenumber: 0 }

    it "should retrieve an address with their id" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo
        void $ DB.runDatabaseT (DB.persistAddress { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo

        address <- DB.runDatabaseT (DB.findAddressById 1) connectionInfo
        let address' = fromMaybe { id: 0, country: "", street: "", city: "", zip: "" } address

        address `shouldSatisfy` isJust
        address' `shouldEqual` { id: 1, country: "France", street: "Street", city: "City", zip: "1234" }

    it "should retrieve a phone with their id" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo
        void $ DB.runDatabaseT (DB.persistPhone { id: 0, prefix: "+33", number: "123456789" }) connectionInfo

        phone <- DB.runDatabaseT (DB.findPhoneById 1) connectionInfo
        let phone' = fromMaybe { id: 0, prefix: "", number: "" } phone

        phone `shouldSatisfy` isJust
        phone' `shouldEqual` { id: 1, prefix: "+33", number: "123456789" }

    it "should retrieve an user with their id" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        address <- DB.runDatabaseT (DB.persistAddress { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }) connectionInfo
        let address' = fromMaybe { id: 0, country: "", street: "", city: "", zip: "" } address
        phone <- DB.runDatabaseT (DB.persistPhone { id: 0, prefix: "+33", number: "123456789" }) connectionInfo
        let phone' = fromMaybe { id: 0, prefix: "", number: "" } phone
        user <- DB.runDatabaseT (DB.persistUser { id: 0, firstname: "John", lastname: "Doe", address: (_.id address'), phonenumber: (_.id phone') }) connectionInfo
        let user' = fromMaybe { id: 0, firstname: "", lastname: "", address: 0, phonenumber: 0 } user

        byId <- DB.runDatabaseT (DB.findUserById user'.id) connectionInfo
        let byId' = fromMaybe { id: 0, firstname: "", lastname: "", address: address', phonenumber: phone' } byId
        byId `shouldSatisfy` isJust
        byId' `shouldNotEqual` { id: 0, firstname: "", lastname: "", address: address', phonenumber: phone' }
        byId'.address.country `shouldEqual` "France"

    it "should persist an user with join" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT DB.createTables connectionInfo

        res <- DB.runDatabaseT
          ( DB.persistUserWithJoin
              { id: 0
              , firstname: "newUser"
              , lastname: "lastName"
              , address:
                  { id: 0
                  , country: "France"
                  , street: "SomeStreet"
                  , city: "Paris"
                  , zip: "92000"
                  }
              , phonenumber:
                  { id: 0
                  , prefix: "+33"
                  , number: "12345"
                  }
              }
          )
          connectionInfo

        res `shouldSatisfy` isJust
        let finalResult = unsafePartial $ fromJust res

        finalResult.id `shouldEqual` 1
        finalResult.address.id `shouldEqual` 1
        finalResult.address.country `shouldEqual` "France"
        finalResult.phonenumber.id `shouldEqual` 1
        finalResult.phonenumber.prefix `shouldEqual` "+33"

    it "should count users" do
      withDatabase \cs -> do
        let connectionInfo = PG.connectionInfoFromString cs
        DB.runDatabaseT
          ( do
              DB.createTables
              void $ DB.persistPhone { id: 0, prefix: "+33", number: "123456789" }
              void $ DB.persistAddress { id: 0, country: "France", street: "Street", city: "City", zip: "1234" }
          )
          connectionInfo

        firstCount <- DB.runDatabaseT DB.countUsers connectionInfo
        traverse_ (\u -> DB.runDatabaseT (DB.persistUser u) connectionInfo)
          [ { id: 0
            , firstname: "John"
            , lastname: "Doe"
            , address: 1
            , phonenumber: 1
            }
          , { id: 0
            , firstname: "Jane"
            , lastname: "Doe"
            , address: 1
            , phonenumber: 1
            }
          ]

        secondCount <- DB.runDatabaseT DB.countUsers connectionInfo
        firstCount `shouldEqual` 0
        secondCount `shouldEqual` 2
