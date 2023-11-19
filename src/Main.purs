module Main where

import Prelude hiding (($>))

import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Argonaut (class DecodeJson, decodeJson, parseJson)
import Data.DateTime (Date, DateTime, Time, date, day, hour, millisecond, minute, month, second, time, year)
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Messages (class FromWsMessage, class ToWsMessage, Request(..), Response(..), fromWsMessage, toWsMessage)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (padStart)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, forkAff, joinFiber, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log, logShow)
import Effect.Now (nowDateTime)
import Effect.Random as R
import Misc.Utils (Radix(..), parseInt)
import Network.WebSocket (WebSocket, close, mkWebSocket, recv, send)
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile)
import Storage.Database as DB
import Storage.Types (Address(..), PhoneNumber(..), User(..))
import Yoga.Postgres as YG

newtype ApplicationEnvironment =
  ApplicationEnvironment { ws :: WebSocket, pool :: YG.Pool }

type ApplicationT a = ReaderT WebSocket (ReaderT YG.Pool Aff) a

-- Fake get requests
data GeneratedRequest
  = GetAddress String
  | GetPhone String
  | GetUser String

mapObjectId :: (String -> String) -> GeneratedRequest -> GeneratedRequest
mapObjectId f (GetAddress id) = GetAddress (f id)
mapObjectId f (GetPhone id) = GetPhone (f id)
mapObjectId f (GetUser id) = GetUser (f id)

getObjectId :: GeneratedRequest -> String
getObjectId (GetAddress id) = id
getObjectId (GetPhone id) = id
getObjectId (GetUser id) = id

infix 5 mapObjectId as $>

instance Show GeneratedRequest where
  show (GetAddress id) = "(GetAddress " <> id <> ")"
  show (GetPhone id) = "(GetPhone " <> id <> ")"
  show (GetUser id) = "(GetUser " <> id <> ")"

type PadSize = Int

-- Converts an ObjectId from an Int to a 8-char length string for interoperation with Hwedis
-- Last byte is reserved for object type (will be set by user)
padObjectId :: PadSize -> Int -> String
padObjectId ps = show >>> padStart ps >>> replaceAll (Pattern " ") (Replacement "0")

-- Reverse of padObjectId
unPadObjectId :: String -> Maybe Int
unPadObjectId = parseInt (Radix 10)

generateRandomRequest :: Effect GeneratedRequest
generateRandomRequest = do
  rnd <- R.randomInt 0 2
  rndId <- R.randomInt 1 1000

  case rnd of
    0 -> pure $ GetAddress (padObjectId 7 rndId)
    1 -> pure $ GetPhone (padObjectId 7 rndId)
    _ -> pure $ GetUser (padObjectId 7 rndId)

runApplication :: ApplicationEnvironment -> ApplicationT ~> Aff
runApplication (ApplicationEnvironment { ws, pool }) m = runReaderT (runReaderT m ws) pool

readUser :: String -> Effect (Array User)
readUser = readDataFile

readPhone :: String -> Effect (Array PhoneNumber)
readPhone = readDataFile

readAddress :: String -> Effect (Array Address)
readAddress = readDataFile

readDataFile :: ∀ a. DecodeJson a => String -> Effect (Array a)
readDataFile path = do
  f <- readFile path >>= toString UTF8
  let parsedResult = parseJson f >>= decodeJson

  case parsedResult of
    Left err -> do
      logShow err
      pure []
    Right x -> pure x

l :: String -> Effect Unit
l logMsg = do
  time <- nowDateTime
  log $ "[" <> showDateTime time <> "] " <> logMsg

showDateTime :: DateTime -> String
showDateTime x =
  showDate (date x) <> " " <> showTime (time x)

  where
  showTime :: Time -> String
  showTime t =
    show (fromEnum $ hour t)
      <> ":"
      <> show (fromEnum $ minute t)
      <> ":"
      <> show (fromEnum $ second t)
      <> "."
      <> show (fromEnum $ millisecond t)

  showDate :: Date -> String
  showDate d =
    show (fromEnum $ year d)
      <> "-"
      <> show (fromEnum $ month d)
      <> "-"
      <> show (fromEnum $ day d)

main :: Effect Unit
main = do
  address <- readAddress "./data/address.json"
  phones <- readPhone "./data/phonenumber.json"
  user <- readUser "./data/users.json"

  launchAff_ do
    ws <- mkWebSocket "ws://127.0.0.1:9092" "hwedis-sdk"
    pool <- liftEffect $ YG.mkPool createConnectionInfo

    liftEffect $ l "Populating database"

    -- Drop tables
    liftEffect $ l "Dropping tables"
    runReaderT DB.dropTables pool

    -- Create tables
    liftEffect $ l "Creating tables"
    runReaderT DB.createTables pool

    -- Populate database with mock data
    liftEffect $ l "Creating addresses"
    ad <- forkAff $ runReaderT (populateDatabase address) pool

    liftEffect $ l "Creating phones"
    ph <- forkAff $ runReaderT (populateDatabase phones) pool

    liftEffect $ l "Joining fibers"
    traverse_ joinFiber [ ad, ph ]

    liftEffect $ l "Creating users"
    runReaderT (populateDatabase user) pool

    { cu, ca, cp } <- do
      cu <- runReaderT DB.countUsers pool
      ca <- runReaderT DB.countAddresses pool
      cp <- runReaderT DB.countPhoneNumbers pool
      pure $ { cu, ca, cp }

    liftEffect $ l $ "Currently having " <> show cu <> " users, " <> show ca <> " addresses, and " <> show cp <> " phone numbers"

    let environment = ApplicationEnvironment { ws, pool }
    liftEffect $ l "Starting application"
    runApplication environment $ mainLoop false

    liftEffect $ l "Stopping application"

    liftEffect $ YG.end pool
    void $ close ws 1000 "Goodbye!"

  where
  createConnectionInfo :: YG.ConnectionInfo
  createConnectionInfo = YG.connectionInfoFromString "postgres://test:test@localhost:5432/test"

  populateDatabase :: ∀ a. DB.Persistable a => DecodeJson a => Array a -> DB.DatabaseT Unit
  populateDatabase d = traverse_ DB.persist d

mainLoop :: Boolean -> ApplicationT Unit
mainLoop stop = when (not stop) $ do
  ws <- ask
  pool <- lift ask

  mainLoop' ws pool 0

mainLoop' :: WebSocket -> YG.Pool -> Int -> ApplicationT Unit
mainLoop' ws pool i = when (i < 150) $ do
  request <- liftEffect $ do
    r <- generateRandomRequest

    let
      letter = case r of
        (GetUser _) -> "U"
        (GetPhone _) -> "P"
        (GetAddress _) -> "A"

      func :: String -> String
      func s = s <> letter
    pure $ func $> r

  cacheQuery <- (queryCache (Get $ getObjectId request) :: ApplicationT (Maybe Response))
    >>= handleCacheResponse request

  case cacheQuery of
      Nothing -> void $ liftEffect $ l $ printIteration <> " Nothing to do"
      Just x -> do
        liftAff $ do
            send ws $ toWsMessage x
            msg <- recv ws
            liftEffect $ l $ printIteration <> " Received: " <> show msg

  liftAff $ delay (Milliseconds 250.0)
  mainLoop' ws pool (i + 1)

  where
  printIteration :: String
  printIteration = "[" <> show i <> "]"

  queryCache :: ∀ a b. ToWsMessage a => FromWsMessage b => a -> ApplicationT (Maybe b)
  queryCache msg = do
    result <- liftAff $ do
      send ws $ toWsMessage msg
      recv ws

    pure $ fromWsMessage result

  queryDb :: ∀ a. DB.Findable a => DecodeJson a => Int -> ApplicationT (Maybe a)
  queryDb objId = do
    liftAff $ runReaderT (DB.find objId) pool

  genObjId :: Int -> String -> String
  genObjId obj letter = (padObjectId 7 obj) <> letter

  storeUserInCache :: User -> Request
  storeUserInCache (User { id, firstname, lastname, address, phonenumber }) = Create (genObjId id "U")
    [ { key: "firstname", value: firstname }
    , { key: "lastname", value: lastname }
    , { key: "address", value: show address }
    , { key: "phonenumber", value: show phonenumber }
    ]

  storeAddressInCache :: Address -> Request
  storeAddressInCache (Address { id, street, zip, city }) = Create (genObjId id "A")
    [ { key: "street", value: street }
    , { key: "zip", value: zip }
    , { key: "city", value: city }
    ]

  storePhoneInCache :: PhoneNumber -> Request
  storePhoneInCache (PhoneNumber { id, number, prefix }) = Create (genObjId id "P")
    [ { key: "number", value: number }
    , { key: "prefix", value: prefix }
    ]

  queryUser :: GeneratedRequest -> ApplicationT (Maybe User)
  queryUser = queryForObj

  queryAddress :: GeneratedRequest -> ApplicationT (Maybe Address)
  queryAddress = queryForObj

  queryPhone :: GeneratedRequest -> ApplicationT (Maybe PhoneNumber)
  queryPhone = queryForObj

  queryForObj :: ∀ a. DB.Findable a => DecodeJson a => GeneratedRequest -> ApplicationT (Maybe a)
  queryForObj req = do
    let id = (getObjectId >>> unPadObjectId) req
    case id of
      Nothing -> pure Nothing
      Just x -> queryDb x

  handleCacheResponse :: GeneratedRequest -> Maybe Response -> ApplicationT (Maybe Request)
  handleCacheResponse _ Nothing = do
    liftEffect $ l "Invalid message received from cache"
    pure Nothing

  handleCacheResponse _ (Just (GetR _ _)) = do
    liftEffect $ l "Object retrieved from cache"
    pure Nothing

  handleCacheResponse req (Just False) = do
    liftEffect $ l "Object not in cache, retrieving from DB"
    case req of
      (GetUser _) -> do
        dbObj <- queryUser req
        case dbObj of
          Nothing -> pure Nothing
          Just x -> pure $ Just $ storeUserInCache x
      (GetAddress _) -> do
        dbObj <- queryAddress req
        case dbObj of
          Nothing -> pure Nothing
          Just x -> pure $ Just $ storeAddressInCache x
      (GetPhone _) -> do
        dbObj <- queryPhone req
        case dbObj of
          Nothing -> pure Nothing
          Just x -> pure $ Just $ storePhoneInCache x

  handleCacheResponse _ (Just _) = do
    liftEffect $ l "Invalid message received from cache, ignoring"
    pure Nothing

