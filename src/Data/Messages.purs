module Data.Messages
  ( Request(..)
  , ObjectId
  , Field
  , class ToWsMessage, toWsMessage
  , class FromWsMessage, fromWsMessage
  , validateRequest
  , mapR
  ) where

--------------------------------------------------------------------------------

import Prelude

import Data.Array (intercalate, uncons)
import Data.Maybe (Maybe(..))
import Data.String (length)

--------------------------------------------------------------------------------

type ObjectId = String
type Field = { key :: String, value :: String }

data Request
    = Get ObjectId
    | List
    | Create ObjectId (Array Field)
    | Update ObjectId (Array Field)
    | Delete ObjectId

class (Show a) <= ToWsMessage a where
  toWsMessage :: a -> String

class (Show a) <= FromWsMessage a where
  fromWsMessage :: String -> a

derive instance Eq Request

instance Show Request where
  show :: Request -> String
  show (List) = "(List)"
  show (Get id) = "(Get " <> id <> ")"
  show (Update id fields) = "(Update " <> id <> " " <> generateFields fields <> ")"
  show (Delete id) = "(Delete " <> id <> ")"
  show (Create id fields) = "(Create " <> id <> " " <> generateFields fields <> ")"

-- Helpers for Show
fieldToString :: Field -> String
fieldToString { key, value } = "[" <> key <> ": " <> value <> "]"

generateFields :: Array Field -> String
generateFields arr =
  let arr' = map fieldToString arr
  in intercalate ", " arr'

-- G::objectid
-- D::objectid
-- U::objectid::field1::value1::field2::value2
-- C::objectid::field1::value1::field2::value2
-- L
instance ToWsMessage Request where
  toWsMessage :: Request -> String
  toWsMessage (List) = "L"
  toWsMessage (Get objid) = "G::" <> objid
  toWsMessage (Update objid fields) = "U::" <> objid <> "::" <> renderFields fields
  toWsMessage (Delete objid) = "D::" <> objid
  toWsMessage (Create objid fields) = "C::" <> objid <> "::" <> renderFields fields

-- Helpers for ToWsMessage
renderFields :: Array Field -> String
renderFields arr =
  let arr' = map (\f -> f.key <> "::" <> f.value) arr
  in intercalate "::" arr'

-- Helpful mapping function for modifying the IDs of objects
mapR :: (ObjectId -> ObjectId) -> Request -> Request
mapR _ List = List
mapR func (Get objid) = Get $ func objid
mapR func (Update objid f) = Update (func objid) f
mapR func (Delete objid) = Delete $ func objid
mapR func (Create objid f) = Create (func objid) f

validateRequest :: Request -> Maybe Request
validateRequest List = Just List
validateRequest (Get o) = do
  ov <- validateObjectId o
  pure $ Get ov
validateRequest (Delete o) = validateObjectId o >>= pure <<< Delete
validateRequest (Update objid f) = do
  ov <- validateObjectId objid
  fields <- validateFields f
  pure $ Update ov fields
validateRequest (Create objid f) = do
  ov <- validateObjectId objid
  fields <- validateFields f
  pure $ Create ov fields

validateObjectId :: ObjectId -> Maybe ObjectId
validateObjectId x
  | length x == 8 = Just x
  | otherwise = Nothing

validateField :: Field -> Maybe Field
validateField f@{key, value} = if length key <= 24 && length value <= 256 then Just f else Nothing

validateFields :: Array Field -> Maybe (Array Field)
validateFields [] = Just []
validateFields [x] = do
  fieldValid <- validateField x
  pure [fieldValid]
validateFields x = do
  let unc = uncons x
  case unc of
    Nothing -> Nothing
    Just {head, tail} -> do
      fieldValid <- validateField head
      res <- validateFields tail
      pure $ [fieldValid] <> res

