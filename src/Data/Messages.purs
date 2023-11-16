module Data.Messages
  ( Request(..)
  , Response(..)
  , ObjectId
  , Field
  , class ToWsMessage
  , toWsMessage
  , class FromWsMessage
  , fromWsMessage
  , validateRequest
  , mapR
  ) where

--------------------------------------------------------------------------------

import Prelude

import Data.Array (intercalate, uncons)
import Data.Maybe (Maybe(..))
import Data.String as String
import Debug (trace)

--------------------------------------------------------------------------------

type ObjectId = String
type Field = { key :: String, value :: String }

data Request
  = Get ObjectId
  | List
  | Create ObjectId (Array Field)
  | Update ObjectId (Array Field)
  | Delete ObjectId

data Response
  = GetR ObjectId (Array Field)
  | CreateR ObjectId
  | UpdateR ObjectId
  | DeleteR ObjectId

class (Show a) <= ToWsMessage a where
  toWsMessage :: a -> String

class (Show a) <= FromWsMessage a where
  fromWsMessage :: String -> Maybe a

derive instance Eq Request
derive instance Eq Response

instance Show Request where
  show :: Request -> String
  show (List) = "(List)"
  show (Get id) = "(Get " <> id <> ")"
  show (Update id fields) = "(Update " <> id <> " " <> generateFields fields <> ")"
  show (Delete id) = "(Delete " <> id <> ")"
  show (Create id fields) = "(Create " <> id <> " " <> generateFields fields <> ")"

instance Show Response where
  show :: Response -> String
  show (GetR id fields) = "(GetR " <> id <> " " <> generateFields fields <> ")"
  show (CreateR id) = "(CreateR " <> id <> ")"
  show (UpdateR id) = "(UpdateR " <> id <> ")"
  show (DeleteR id) = "(DeleteR " <> id <> ")"

-- Helpers for Show
fieldToString :: Field -> String
fieldToString { key, value } = "[" <> key <> ": " <> value <> "]"

generateFields :: Array Field -> String
generateFields arr =
  let
    arr' = map fieldToString arr
  in
    intercalate ", " arr'

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

instance FromWsMessage Response where
  fromWsMessage :: String -> Maybe Response
  fromWsMessage msg =
    let
      f :: String -> Maybe { head :: String, tail :: Array String }
      f = uncons <<< toSplitArray

    in
      do
        { head: x, tail: xs } <- f msg
        { head: xr, tail: xsr } <- uncons xs
        validObjectId <- validateObjectId xr

        case x of
          "G" -> do
            fields <- splitFields xsr
            validFields <- validateFields fields
            pure $ GetR xr validFields
          "U" -> pure $ UpdateR validObjectId
          "D" -> pure $ DeleteR validObjectId
          "C" -> pure $ CreateR validObjectId
          _ -> Nothing

    where
    toSplitArray :: String -> Array String
    toSplitArray = String.split (String.Pattern "::")

    splitFields :: Array String -> Maybe (Array Field)
    splitFields [] = pure []
    splitFields arr = do
      { head: key, tail: rest } <- uncons arr
      { head: value, tail: xs } <- uncons rest

      pure [ { key, value } ] <> splitFields xs

-- Helpers for ToWsMessage
renderFields :: Array Field -> String
renderFields arr =
  let
    arr' = map (\f -> f.key <> "::" <> f.value) arr
  in
    intercalate "::" arr'

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
  | String.length x == 8 = Just x
  | otherwise = Nothing

validateField :: Field -> Maybe Field
validateField f@{ key, value } = if String.length key <= 24 && String.length value <= 256 then Just f else Nothing

validateFields :: Array Field -> Maybe (Array Field)
validateFields [] = Nothing
validateFields [ x ] = do
  fieldValid <- validateField x
  pure [ fieldValid ]
validateFields x = do
  let unc = uncons x
  case unc of
    Nothing -> Nothing
    Just { head, tail } -> do
      fieldValid <- validateField head
      res <- validateFields tail
      pure $ [ fieldValid ] <> res

