module Main where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, put)
import Data.Argonaut (class DecodeJson, decodeJson, parseJson)
import Data.Array (foldr, (..))
import Data.Configuration as Config
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Random (randomInt)
import Monad.Bench (AppM, CacheStats, AppState, getCache, getDb, log, putCache, runAppM, updateCache, updateDb)
import Network.WebSocket as WS
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile)
import Storage.Database as DB
import Storage.Types (Address(..), PhoneNumber(..), User(..))
import Yoga.Postgres as YG

data FoundWhere a
  = Cache a
  | Db a
  | Nowhere

instance Functor FoundWhere where
  map :: ∀ a b. (a -> b) -> FoundWhere a -> FoundWhere b
  map _ Nowhere = Nowhere
  map f (Cache a) = Cache (f a)
  map f (Db a) = Db (f a)

getUserFromCacheOrDb :: Int -> AppM (FoundWhere User)
getUserFromCacheOrDb id = do
  { ws, pool, configuration: { db: { simulation: { minDelay, maxDelay } } } } <- ask
  user <- getCache ws id

  case user of
    Nothing -> do
      log "Could not find user in cache, reverting to DB"
      fromDb <- getDb minDelay maxDelay pool id
      case fromDb of
        Nothing -> pure Nowhere
        Just x -> pure $ Db x

    Just x -> do
      log "User found in cache"
      pure $ Cache x

getAddressFromCacheOrDb :: Int -> AppM (FoundWhere Address)
getAddressFromCacheOrDb id = do
  { ws, pool, configuration: { db: { simulation: { minDelay, maxDelay } } } } <- ask
  address <- getCache ws id

  case address of
    Nothing -> do
      log "Could not find address in cache, reverting to DB"
      fromDb <- getDb minDelay maxDelay pool id
      case fromDb of
        Nothing -> pure Nowhere
        Just x -> pure $ Db x
    Just x -> do
      log "Address found in cache"
      pure $ Cache x

getPhoneFromCacheOrDb :: Int -> AppM (FoundWhere PhoneNumber)
getPhoneFromCacheOrDb id = do
  { ws, pool, configuration: { db: { simulation: { minDelay, maxDelay } } } } <- ask
  phone <- getCache ws id

  case phone of
    Nothing -> do
      log "Could not find phone in cache, reverting to DB"
      fromDb <- getDb minDelay maxDelay pool id
      case fromDb of
        Nothing -> pure Nowhere
        Just x -> pure $ Db x
    Just x -> do
      log "Phone found in cache"
      pure $ Cache x

data FakeRequest
  = GetUser Int
  | GetPhone Int
  | GetAddress Int
  | UpdateUser Int
  | UpdatePhone Int
  | UpdateAddress Int

generateRandomRequest :: ∀ m. (MonadEffect m) => m FakeRequest
generateRandomRequest = do
  { id, req } <- liftEffect $ do
    id <- randomInt 1 1000
    req <- randomInt 1 6
    pure { id, req }

  case req of
    1 -> pure $ GetUser id
    2 -> pure $ GetPhone id
    3 -> pure $ GetAddress id
    4 -> pure $ UpdateUser id
    5 -> pure $ UpdatePhone id
    _ -> pure $ UpdateAddress id

readUser :: ∀ m. MonadEffect m => String -> m (Array User)
readUser = readDataFile

readPhone :: ∀ m. MonadEffect m => String -> m (Array PhoneNumber)
readPhone = readDataFile

readAddress :: ∀ m. MonadEffect m => String -> m (Array Address)
readAddress = readDataFile

readDataFile :: ∀ a m. MonadEffect m => DecodeJson a => String -> m (Array a)
readDataFile path = do
  f <- liftEffect $ readFile path >>= toString UTF8
  let parsedResult = parseJson f >>= decodeJson

  case parsedResult of
    Left err -> do
      liftEffect $ Console.log $ show err
      pure []
    Right x -> pure x

program :: AppM Unit
program = do
  s <- get
  e@{ configuration: { iterationsPerWorker } } <- ask

  -- Start the receiver in a separate Aff context
  fiber <- liftAff $ forkAff (runAppM receiver s e)

  runLoop iterationsPerWorker 0

  -- Kill the receiver
  liftAff $ killFiber (error "I HATE YOU!") fiber

  where
  putCacheHit :: CacheStats -> CacheStats
  putCacheHit { cacheHit, cacheMiss } = { cacheHit: cacheHit + 1, cacheMiss }

  putCacheMiss :: CacheStats -> CacheStats
  putCacheMiss { cacheHit, cacheMiss } = { cacheHit, cacheMiss: cacheMiss + 1 }

  runLoop :: Int -> Int -> AppM Unit
  runLoop limit count = when (count < limit) $ do
    { ws, pool, configuration: { db: { simulation: { minDelay, maxDelay } } } } <- ask
    { users, addresses, phones } <- get

    log $ "Iteration #" <> show count

    randomRequest <- generateRandomRequest
    case randomRequest of
      GetUser id -> do
        u <- getUserFromCacheOrDb id
        case u of
          Nowhere -> pure unit
          Cache _ -> put { users: putCacheHit users, addresses, phones }
          Db x -> do
            put { users: putCacheMiss users, addresses, phones }
            putCache ws x
      GetAddress id -> do
        a <- getAddressFromCacheOrDb id
        case a of
          Nowhere -> pure unit
          Cache _ -> put { users, addresses: putCacheHit addresses, phones }
          Db x -> do
            put { users, addresses: putCacheMiss addresses, phones }
            putCache ws x
      GetPhone id -> do
        p <- getPhoneFromCacheOrDb id
        case p of
          Nowhere -> pure unit
          Cache _ -> put { users, addresses, phones: putCacheHit phones }
          Db x -> do
            put { users, addresses, phones: putCacheMiss phones }
            putCache ws x
      UpdateUser id -> do
        u <- getUserFromCacheOrDb id
        case u of
          Nowhere -> pure unit
          Cache (User { id: uid, firstname, lastname, address, phonenumber }) -> do
            let newUser = User { id: uid, firstname: firstname <> " modified", lastname: lastname <> " modified", address, phonenumber }
            updateDb minDelay maxDelay pool newUser
            updateCache ws newUser
            put { users: putCacheHit users, addresses, phones }
          Db (User { id: uid, firstname, lastname, address, phonenumber }) -> do
            let newUser = User { id: uid, firstname: firstname <> " modified", lastname: lastname <> " modified", address, phonenumber }
            updateDb minDelay maxDelay pool newUser
            putCache ws newUser
            put { users: putCacheMiss users, addresses, phones }
      UpdateAddress id -> do
        a <- getAddressFromCacheOrDb id
        case a of
          Nowhere -> pure unit
          Cache (Address { id: aid, street, city, zip, country }) -> do
            let newAddress = Address { id: aid, street: street <> " modified", city: city <> " modified", zip, country }
            updateDb minDelay maxDelay pool newAddress
            updateCache ws newAddress
            put { users, addresses: putCacheHit addresses, phones }
          Db (Address { id: aid, street, city, zip, country }) -> do
            let newAddress = Address { id: aid, street: street <> " modified", city: city <> " modified", zip, country }
            updateDb minDelay maxDelay pool newAddress
            putCache ws newAddress
            put { users, addresses: putCacheMiss addresses, phones }
      UpdatePhone id -> do
        p <- getPhoneFromCacheOrDb id
        case p of
          Nowhere -> pure unit
          Cache (PhoneNumber { id: pid, number, prefix }) -> do
            let newPhone = PhoneNumber { id: pid, number: number <> " modified", prefix }
            updateDb minDelay maxDelay pool newPhone
            updateCache ws newPhone
            put { users, addresses, phones: putCacheHit phones }
          Db (PhoneNumber { id: pid, number, prefix }) -> do
            let newPhone = PhoneNumber { id: pid, number: number <> " modified", prefix }
            updateDb minDelay maxDelay pool newPhone
            putCache ws newPhone
            put { users, addresses, phones: putCacheMiss phones }

    runLoop limit (count + 1)

-- | Receiver: this function runs in an Aff context, always listening for messages coming from
-- | the websocket and modifying the state in consequence. This fiber will never end.
receiver :: AppM Unit
receiver = forever $ do
  { ws } <- ask
  msg <- liftAff $ WS.recv ws
  log $ "Received: " <> show msg

newtype Iterations = Iterations Int
newtype ClientId = ClientId String

main :: Effect Unit
main = launchAff_ $ do
  conf <- liftEffect $ Config.parse "bencher.toml"

  case conf of
    Left err -> do
      liftEffect $ Console.log $ "An error occured while loading the configuration: " <> show err
      pure unit
    Right config -> do
      -- Read data files
      usersFromFile <- readUser "./data/users.json"
      addressesFromFile <- readAddress "./data/address.json"
      phonesFromFile <- readPhone "./data/phonenumber.json"

      pool <- liftEffect $ YG.mkPool $ createConnectionInfo config

      liftEffect $ Console.log "Populating database"
      flip runReaderT pool $ do
        DB.dropTables
        DB.createTables

        traverse_ DB.persist addressesFromFile
        traverse_ DB.persist phonesFromFile
        traverse_ DB.persist usersFromFile

      liftEffect $ YG.end pool

      arrFiber <- traverse (bencher config >>> forkAff) (1 .. config.workers)
      arrResults <- traverse joinFiber arrFiber

      liftEffect $ Console.log "Benchmarking over, collecting and aggregating results..."

      -- Print statistics:
      liftEffect $ printStats (aggregateStats $ map (\(Tuple _ stats) -> stats) arrResults)

  where
  createConnectionInfo :: Config.Configuration -> YG.ConnectionInfo
  createConnectionInfo { db: { host, port, user, password, database } } =
    YG.connectionInfoFromString $ "postgres://" <> user <> ":" <> password <> "@" <> host <> ":" <> show port <> "/" <> database

  bencher :: Config.Configuration -> Int -> Aff (Tuple Unit AppState)
  bencher config@{ hwedis: { clientPrefix } } ix = do
    let initStatus = { cacheHit: 0, cacheMiss: 0 }
    let st = { users: initStatus, addresses: initStatus, phones: initStatus }

    newWs <- WS.mkWebSocket ("ws://" <> config.hwedis.host <> ":" <> show config.hwedis.port) (clientPrefix <> show ix)
    newPool <- liftEffect $ YG.mkPool $ createConnectionInfo config

    res <- runAppM program st { ws: newWs, pool: newPool, configuration: config }

    void $ WS.close newWs 1000 "Goodbye!"
    liftEffect $ YG.end newPool
    pure res

  aggregateStats :: Array AppState -> AppState
  aggregateStats arr =
    let
      sumStats :: CacheStats -> CacheStats -> CacheStats
      sumStats { cacheHit: aCh, cacheMiss: aCm } { cacheHit: bCh, cacheMiss: bCm } = { cacheHit: aCh + bCh, cacheMiss: aCm + bCm }

      sumFull :: AppState -> AppState -> AppState
      sumFull { users: au, addresses: aa, phones: ap } { users: bu, addresses: ba, phones: bp } =
        { users: sumStats au bu, addresses: sumStats aa ba, phones: sumStats ap bp }

      statsZero = { cacheHit: 0, cacheMiss: 0 }
    in
      foldr sumFull { users: statsZero, addresses: statsZero, phones: statsZero } arr

  printStats :: AppState -> Effect Unit
  printStats { users, addresses, phones } = do
    Console.log $ "_________________________________________"
    Console.log $ " Users:"
    Console.log $ "   - Hit  : " <> show users.cacheHit
    Console.log $ "   - Miss : " <> show users.cacheMiss
    Console.log $ "   - Total: " <> show (users.cacheHit + users.cacheMiss)
    Console.log ""
    Console.log $ " Addresses:"
    Console.log $ "   - Hit  : " <> show addresses.cacheHit
    Console.log $ "   - Miss : " <> show addresses.cacheMiss
    Console.log $ "   - Total: " <> show (addresses.cacheHit + addresses.cacheMiss)
    Console.log ""
    Console.log $ " Phone Numbers:"
    Console.log $ "   - Hit  : " <> show phones.cacheHit
    Console.log $ "   - Miss : " <> show phones.cacheMiss
    Console.log $ "   - Total: " <> show (phones.cacheHit + phones.cacheMiss)
    Console.log $ "________________________________________"
    Console.log $ " Total Hits  : " <> show (users.cacheHit + addresses.cacheHit + phones.cacheHit)
    Console.log $ " Total Misses: " <> show (users.cacheMiss + addresses.cacheMiss + phones.cacheMiss)
    Console.log $ "________________________________________"
    Console.log $ " # Iterations: " <> show (users.cacheHit + users.cacheMiss + addresses.cacheHit + addresses.cacheMiss + phones.cacheHit + phones.cacheMiss)

