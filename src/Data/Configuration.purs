module Data.Configuration
  ( Configuration
  , DbLookup
  , DbConfiguration
  , HwedisConfiguration
  , defaultConfiguration
  , FilePath
  , TomlParseError
  , parse
  ) where

import Prelude

import Data.Argonaut (Json, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error, catchException, message)
import Misc.Logger (Severity(..))
import Node.Buffer (toString)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readFile)

type FilePath = String
type TomlParseError = String
type TomlContent = String

type EitherParse = Either TomlParseError Json

foreign import parseImpl :: ∀ a b. TomlContent -> (a -> EitherParse) -> (b -> EitherParse) -> Effect EitherParse

type Configuration =
  { workers :: Int
  , iterationsPerWorker :: Int
  , db :: DbConfiguration
  , hwedis :: HwedisConfiguration
  , logLevel :: Severity
  }

type DbConfiguration =
  { host :: String
  , port :: Int
  , user :: String
  , password :: String
  , database :: String
  , simulation :: DbLookup
  }

type DbLookup =
  { minDelay :: Int
  , maxDelay :: Int
  }

type HwedisConfiguration =
  { host :: String
  , port :: Int
  , clientPrefix :: String
  }

defaultConfiguration :: Configuration
defaultConfiguration =
  { workers: 5
  , iterationsPerWorker: 500
  , db:
      { host: "localhost"
      , port: 5432
      , user: "test"
      , password: "test"
      , database: "test"
      , simulation: { minDelay: 15, maxDelay: 250 }
      }
  , hwedis:
      { host: "localhost"
      , port: 9092
      , clientPrefix: "bencher"
      }
  , logLevel: Info
  }

parse :: FilePath -> Effect (Either TomlParseError Configuration)
parse fp = do
  content <- catchException (pure <<< handleNoEnt) (readFile fp >>= toString UTF8 >>= Right >>> pure)
  case content of
    Left err -> pure $ Left err
    Right str -> do
      p <- parseImpl str Left Right

      case p of
        Left err -> pure $ Left err
        Right x -> pure $ jsonDecode x

  where
  jsonDecode :: Json -> Either TomlParseError Configuration
  jsonDecode = decodeJson >>> lmap printJsonDecodeError

  handleNoEnt :: Error -> Either TomlParseError String
  handleNoEnt = Left <<< message
