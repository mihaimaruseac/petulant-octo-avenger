module Errors where

import Control.Monad.Error
import Data.Word

import Types

data StatsError
  = IncompleteCapture Word32 Word32 -- wire length, capture length
  | FragmentationError
  | OtherError String
  | UndefinedLayer3Protocol
  | UnhandledParseIP
  | UnhandledParseTCP
  | MoreRequestsInConversation
  | UnexpectedHTTPRequest Payload
  | HTTPError Payload Payload
  | UnacceptableEncoding Payload Payload

instance Error StatsError where
  strMsg s = OtherError s

instance Show StatsError where
  show (IncompleteCapture wl cl) = "# Incomplete capture: " ++ show (wl, cl)
  show (OtherError s) = s
  show FragmentationError = "# Unable to handle fragmentation at IP level"
  show UndefinedLayer3Protocol = "# Undefined layer 3 proto"
  show UnhandledParseIP = "# Unhandled parseIP case"
  show UnhandledParseTCP = "# Unhandled parseTCP case"
  show MoreRequestsInConversation = "# One request only assumption failed"
  show (UnexpectedHTTPRequest t) = "# Unknown/unexpected request " ++ show t
  show (HTTPError u r) = concat ["# Request to ", show u, " failed ", show r]
  show (UnacceptableEncoding h h1) = concat ["# Unacceptable encoding ", show h, " / ", show h1]

type StatsM = Either StatsError
