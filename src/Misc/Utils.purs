module Misc.Utils(parseInt, Radix(..)) where

import Prelude

import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)

foreign import parseIntImpl :: String -> Int -> Number

newtype Radix = Radix Int

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


