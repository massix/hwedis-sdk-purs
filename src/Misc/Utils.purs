module Misc.Utils(parseInt, Radix(..), padId) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.String.Utils (padStart)

foreign import parseIntImpl :: String -> Int -> Number

newtype Radix = Radix Int

padId :: Int -> String
padId = replaceAll (Pattern " ") (Replacement "0") <<< padStart 7 <<< show

parseInt :: Radix -> String -> Maybe Int
parseInt (Radix r) n =
  let
    result = parseIntImpl n r
  in
    if r >= 2 && r <= 36 then
      if isNaN result then
        Nothing
      else
        Just $ round result
    else
      Nothing


