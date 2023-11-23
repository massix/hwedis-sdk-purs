module Data.Serializable
  ( class Deserializable
  , class Serializable
  , deserialize
  , serialize
  ) where

import Prelude

import Data.Array (filter, head)
import Data.Maybe (Maybe(..))
import Data.Messages as Cache
import Misc.Utils (Radix(..), padId, parseInt)
import Storage.Types (Address(..), PhoneNumber(..), User(..))

getField :: String -> Array Cache.Field -> Maybe Cache.Field
getField fn = filter (_.key >>> ((==) fn)) >>> head

class Deserializable a where
  deserialize :: Cache.Response -> Maybe a

class Serializable a where
  serialize :: (Cache.ObjectId -> Array Cache.Field -> Cache.Request) -> a -> Cache.Request

instance Deserializable User where
  deserialize (Cache.GetR objectId fields) = do
    firstname <- extractField "firstname" fields
    lastname <- extractField "lastname" fields
    address <- extractField "address" fields >>= parseInt (Radix 10)
    phonenumber <- extractField "phonenumber" fields >>= parseInt (Radix 10)
    cacheId <- parseInt (Radix 10) objectId

    pure $ User { id: cacheId, firstname, lastname, phonenumber, address }
    where
    extractField :: String -> Array Cache.Field -> Maybe String
    extractField key arr = getField key arr >>= _.value >>> pure
  deserialize _ = Nothing

instance Serializable User where
  serialize c u@(User { id }) =
    let
      objectId = padId id <> "U"
    in
      c objectId (mkFields u)
    where
    mkFields :: User -> Array Cache.Field
    mkFields (User { firstname, lastname, address, phonenumber }) =
      [ { key: "firstname", value: firstname }
      , { key: "lastname", value: lastname }
      , { key: "address", value: show address }
      , { key: "phonenumber", value: show phonenumber }
      ]

instance Deserializable PhoneNumber where
  deserialize (Cache.GetR objectId fields) = do
    number <- extractField "number" fields
    prefix <- extractField "prefix" fields
    cacheId <- parseInt (Radix 10) objectId

    pure $ PhoneNumber { id: cacheId, number, prefix }
    where
    extractField :: String -> Array Cache.Field -> Maybe String
    extractField key arr = getField key arr >>= _.value >>> pure

  deserialize _ = Nothing

instance Serializable PhoneNumber where
  serialize c p@(PhoneNumber { id }) =
    let
      objectId = padId id <> "P"
    in
      c objectId (mkFields p)

    where
    mkFields :: PhoneNumber -> Array Cache.Field
    mkFields (PhoneNumber { number, prefix }) =
      [ { key: "number", value: number }
      , { key: "prefix", value: prefix }
      ]

instance Deserializable Address where
  deserialize (Cache.GetR objectId fields) = do
    country <- extractField "country" fields
    city <- extractField "city" fields
    street <- extractField "street" fields
    zip <- extractField "zip" fields
    cacheId <- parseInt (Radix 10) objectId
    pure $ Address { id: cacheId, country, city, street, zip }

    where
    extractField :: String -> Array Cache.Field -> Maybe String
    extractField key arr = getField key arr >>= _.value >>> pure

  deserialize _ = Nothing

instance Serializable Address where
  serialize c a@(Address { id }) =
    let
      objectId = padId id <> "A"
    in
      c objectId (mkFields a)

    where
    mkFields :: Address -> Array Cache.Field
    mkFields (Address { country, city, street, zip }) =
      [ { key: "country", value: country }
      , { key: "city", value: city }
      , { key: "street", value: street }
      , { key: "zip", value: zip }
      ]

