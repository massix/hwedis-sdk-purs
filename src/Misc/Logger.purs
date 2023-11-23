module Misc.Logger where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson)
import Data.DateTime (date, day, hour, millisecond, minute, month, second, time, year)
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, fromEnum)
import Data.String (Pattern(..), Replacement(..), replaceAll, toUpper)
import Data.String.Utils (padStart)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now (nowDateTime)

type PadSize = Int

data Severity
  = Debug
  | Info
  | Warning
  | Error

derive instance Eq Severity
derive instance Ord Severity

instance Show Severity where
  show Debug = "Debug"
  show Info = "Info"
  show Warning = "Warning"
  show Error = "Error"

instance DecodeJson Severity where
  decodeJson :: Json -> Either JsonDecodeError Severity
  decodeJson j = do
    str <- decodeJson j
    case str of
      "Debug" -> Right Debug
      "Info" -> Right Info
      "Warning" -> Right Warning
      "Error" -> Right Error
      _ -> Left $ UnexpectedValue j

instance Bounded Severity where
  top = Error
  bottom = Debug

class (MonadEffect m) <= MonadLogger m where
  getContext :: m String
  getLogLevel :: m Severity

instance MonadLogger Effect where
  getContext = pure "main"
  getLogLevel = pure Info

instance MonadLogger Aff where
  getContext = pure "async"
  getLogLevel = pure Info

-- Transform a string "s" in "[datetime] context level s
log :: ∀ m. MonadLogger m => Severity -> String -> m Unit
log sev x = do
  monadSeverity <- getLogLevel
  if sev >= monadSeverity then
    prependSeverity sev x >>= prependContext >>= prependDateTime >>= Console.log >>> liftEffect
  else
    pure unit

  where
  prependSeverity :: Severity -> String -> m String
  prependSeverity Debug orig = pure $ "\x1b[1;35mDEBUG\x1b[0m " <> orig
  prependSeverity Info orig = pure $ "\x1b[1;34mINFO\x1b[0m " <> orig
  prependSeverity Warning orig = pure $ "\x1b[1;33mWARNING\x1b[0m " <> orig
  prependSeverity Error orig = pure $ "\x1b[1;31mERROR\x1b[0m " <> orig

  prependContext :: String -> m String
  prependContext orig = do
     context <- getContext
     pure $ "\x1b[1;32m" <> toUpper context <> "\x1b[0m " <> orig

  prependDateTime :: String -> m String
  prependDateTime s = do
    n <- liftEffect $ nowDateTime
    let
      nd = date n
      nt = time n

      ge :: ∀ a. BoundedEnum a => PadSize -> a -> String
      ge ps = replaceAll (Pattern " ") (Replacement "0") <<< padStart ps <<< show <<< fromEnum

      showDate = ge 4 (year nd) <> "-" <> ge 2 (month nd) <> "-" <> ge 2 (day nd)
      showTime = ge 2 (hour nt) <> ":" <> ge 2 (minute nt) <> ":" <> ge 2 (second nt) <> "." <> ge 4 (millisecond nt)

    pure $ "[" <> showDate <> " " <> showTime <> "] " <> s
