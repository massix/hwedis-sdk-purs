module Storage.Types where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError, decodeJson, (.:))
import Data.Either (Either)

-- This is our "model"
newtype User = User
  { id :: Int
  , firstname :: String
  , lastname :: String
  , phonenumber :: Int -- ^ Foreign Key
  , address :: Int -- ^ Foreign Key
  }

newtype PhoneNumber = PhoneNumber
  { id :: Int
  , prefix :: String
  , number :: String
  }

newtype Address = Address
  { id :: Int
  , country :: String
  , city :: String
  , street :: String
  , zip :: String
  }

newtype UserWithJoin = UserWithJoin
  { id :: Int
  , firstname :: String
  , lastname :: String
  , phonenumber :: PhoneNumber -- ^ Foreign Key
  , address :: Address -- ^ Foreign Key
  }

derive newtype instance Show User
derive newtype instance Show Address
derive newtype instance Show PhoneNumber
derive newtype instance Show UserWithJoin

instance DecodeJson User where
  decodeJson :: Json -> Either JsonDecodeError User
  decodeJson j = do
    obj <- decodeJson j
    id <- obj .: "id"
    firstname <- obj .: "firstname"
    lastname <- obj .: "lastname"
    phonenumber <- obj .: "phonenumber"
    address <- obj .: "address"

    pure $ User { id, firstname, lastname, phonenumber, address }

instance DecodeJson Address where
  decodeJson :: Json -> Either JsonDecodeError Address
  decodeJson j = do
    obj <- decodeJson j
    id <- obj .: "id"
    country <- obj .: "country"
    city <- obj .: "city"
    street <- obj .: "street"
    zip <- obj .: "zip"

    pure $ Address { id, country, city, street, zip }

instance DecodeJson PhoneNumber where
  decodeJson :: Json -> Either JsonDecodeError PhoneNumber
  decodeJson j = do
    obj <- decodeJson j
    id <- obj .: "id"
    number <- obj .: "number"
    prefix <- obj .: "prefix"

    pure $ PhoneNumber { id, number, prefix }

