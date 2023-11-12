module Network.WebSocket
  ( WebSocket
  , mkWebSocket
  , send
  , close
  , recv
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

data WebSocket

-- FFI Imports
foreign import mkWebSocketImpl :: String -> String -> Effect (Promise WebSocket)
foreign import sendImpl :: WebSocket -> String -> Effect (Promise Unit)
foreign import closeImpl :: WebSocket -> Int -> String -> Effect (Promise Int)
foreign import recvImpl :: WebSocket -> Effect (Promise String)

-- PURS Implementations

-- | Creates a new WebSocket, first param is the URL, second is the User-Agent to set
mkWebSocket :: String -> String -> Aff WebSocket
mkWebSocket url id = toAffE $ mkWebSocketImpl url id

-- | Sends the String message using the WebSocket
send :: WebSocket -> String -> Aff Unit
send ws msg = toAffE $ sendImpl ws msg

-- | Closes the WebSocket
close :: WebSocket -> Int -> String -> Aff Int
close ws code reason = toAffE $ closeImpl ws code reason

-- | Receive a message, this function blocks until a message is available
recv :: WebSocket -> Aff String
recv = recvImpl >>> toAffE
