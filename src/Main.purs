module Main where

import Prelude

import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, put)
import Data.Argonaut (class DecodeJson, decodeJson, parseJson)
import Data.Array (foldr, (..))
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, forkAff, joinFiber, killFiber, launchAff_)
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

data FoundWhere a = Cache a | Db a | Nowhere

getUserFromCacheOrDb :: Int -> AppM (FoundWhere User)
getUserFromCacheOrDb id = do
  { ws, pool } <- ask
  user <- getCache ws id

  case user of
    Nothing -> do
      log "Could not find user in cache, reverting to DB"
      fromDb <- getDb pool id
      case fromDb of
        Nothing -> pure Nowhere
        Just x -> pure $ Db x

    Just x -> do
      log "User found in cache"
      pure $ Cache x

getAddressFromCacheOrDb :: Int -> AppM (FoundWhere Address)
getAddressFromCacheOrDb id = do
  { ws, pool } <- ask
  address <- getCache ws id

  case address of
    Nothing -> do
      log "Could not find address in cache, reverting to DB"
      fromDb <- getDb pool id
      case fromDb of
        Nothing -> pure Nowhere
        Just x -> pure $ Db x
    Just x -> do
      log "Address found in cache"
      pure $ Cache x

getPhoneFromCacheOrDb :: Int -> AppM (FoundWhere PhoneNumber)
getPhoneFromCacheOrDb id = do
  { ws, pool } <- ask
  phone <- getCache ws id

  case phone of
    Nothing -> do
      log "Could not find phone in cache, reverting to DB"
      fromDb <- getDb pool id
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
  e@{ iterations } <- ask

  -- Start the receiver in a separate Aff context
  fiber <- liftAff $ forkAff (runAppM receiver s e)

  runLoop iterations 0

  -- Kill the receiver
  liftAff $ killFiber (error "I HATE YOU!") fiber

  where
  putCacheHit :: CacheStats -> CacheStats
  putCacheHit { cacheHit, cacheMiss } = { cacheHit: cacheHit + 1, cacheMiss }

  putCacheMiss :: CacheStats -> CacheStats
  putCacheMiss { cacheHit, cacheMiss } = { cacheHit, cacheMiss: cacheMiss + 1 }

  runLoop :: Int -> Int -> AppM Unit
  runLoop limit count = when (count < limit) $ do
    { ws, pool } <- ask
    { users, addresses, phones } <- get

    log $ "Iteration #" <> show count

    randomRequest <- generateRandomRequest
    case randomRequest of
      GetUser id -> do
        u <- getUserFromCacheOrDb id
        case u of
          Nowhere -> pure unit
          Cache x -> do
            put { users: putCacheHit users, addresses, phones }
            log $ "Cache: " <> show x
          Db x -> do
            put { users: putCacheMiss users, addresses, phones }
            log $ "DB: " <> show x
            putCache ws x
      GetAddress id -> do
        a <- getAddressFromCacheOrDb id
        case a of
          Nowhere -> pure unit
          Cache x -> do
            put { users, addresses: putCacheHit addresses, phones }
            log $ "Cache: " <> show x
          Db x -> do
            put { users, addresses: putCacheMiss addresses, phones }
            log $ "DB: " <> show x
            putCache ws x
      GetPhone id -> do
        p <- getPhoneFromCacheOrDb id
        case p of
          Nowhere -> pure unit
          Cache x -> do
            put { users, addresses, phones: putCacheHit phones }
            log $ "Cache: " <> show x
          Db x -> do
            put { users, addresses, phones: putCacheMiss phones }
            log $ "DB: " <> show x
            putCache ws x
      UpdateUser id -> do
        log $ "Updating user with id: " <> show id
        u <- getUserFromCacheOrDb id
        case u of
          Nowhere -> log "Could not find user to update"
          Cache (User { id: uid, firstname, lastname, address, phonenumber }) -> do
            let newUser = User { id: uid, firstname: firstname <> " modified", lastname: lastname <> " modified", address, phonenumber }
            log "User was in cache, updating"
            updateDb pool newUser
            updateCache ws newUser
            put { users: putCacheHit users, addresses, phones }
          Db (User { id: uid, firstname, lastname, address, phonenumber }) -> do
            let newUser = User { id: uid, firstname: firstname <> " modified", lastname: lastname <> " modified", address, phonenumber }
            log "User was in DB, creating new entry"
            updateDb pool newUser
            putCache ws newUser
            put { users: putCacheMiss users, addresses, phones }
      UpdateAddress id -> do
        log $ "Updating address with id: " <> show id
        a <- getAddressFromCacheOrDb id
        case a of
          Nowhere -> log "Could not find address to update"
          Cache (Address { id: aid, street, city, zip, country }) -> do
            let newAddress = Address { id: aid, street: street <> " modified", city: city <> " modified", zip, country }
            updateDb pool newAddress
            updateCache ws newAddress
            put { users, addresses: putCacheHit addresses, phones }
          Db (Address { id: aid, street, city, zip, country }) -> do
            let newAddress = Address { id: aid, street: street <> " modified", city: city <> " modified", zip, country }
            updateDb pool newAddress
            putCache ws newAddress
            put { users, addresses: putCacheMiss addresses, phones }
      UpdatePhone id -> do
        log $ "Updating phone with id: " <> show id
        p <- getPhoneFromCacheOrDb id
        case p of
          Nowhere -> log "Could not find phone to update"
          Cache (PhoneNumber { id: pid, number, prefix }) -> do
            let newPhone = PhoneNumber { id: pid, number: number <> " modified", prefix }
            updateDb pool newPhone
            updateCache ws newPhone
            put { users, addresses, phones: putCacheHit phones }
          Db (PhoneNumber { id: pid, number, prefix }) -> do
            let newPhone = PhoneNumber { id: pid, number: number <> " modified", prefix }
            updateDb pool newPhone
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
  let iterations = 500

  -- Read data files
  usersFromFile <- readUser "./data/users.json"
  addressesFromFile <- readAddress "./data/address.json"
  phonesFromFile <- readPhone "./data/phonenumber.json"

  pool <- liftEffect $ YG.mkPool createConnectionInfo

  liftEffect $ Console.log "Populating database"
  flip runReaderT pool $ do
    DB.dropTables
    DB.createTables

    traverse_ DB.persist addressesFromFile
    traverse_ DB.persist phonesFromFile
    traverse_ DB.persist usersFromFile

  liftEffect $ YG.end pool

  let
    benchFunction c = do
      delay (Milliseconds 250.0)
      forkAff $ bencher (Iterations iterations) (ClientId $ "Client" <> show c)

  arrFiber <- traverse benchFunction (1 .. 5)
  arrResults <- traverse joinFiber arrFiber

  -- Print statistics:
  liftEffect $ printStats (aggregateStats $ map (\(Tuple _ stats) -> stats) arrResults)

  where
  createConnectionInfo :: YG.ConnectionInfo
  createConnectionInfo = YG.connectionInfoFromString "postgres://test:test@localhost:5432/test"

  bencher :: Iterations -> ClientId -> Aff (Tuple Unit AppState)
  bencher (Iterations i) (ClientId c) = do
    let initStatus = { cacheHit: 0, cacheMiss: 0 }
    let st = { users: initStatus, addresses: initStatus, phones: initStatus }

    newWs <- WS.mkWebSocket "ws://127.0.0.1:9092" c
    newPool <- liftEffect $ YG.mkPool createConnectionInfo

    res <- runAppM program st { ws: newWs, pool: newPool, iterations: i }

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

