module Errors where

import Control.Monad.Error
import Data.Word

import Types
import TCP (Port)

data StatsError
  = IncompleteCapture Word32 Word32 -- wire length, capture length
  | FragmentationError
  | HTTPError Payload Payload
  | MoreRequestsInConversation
  | OtherError String
  | UnacceptableEncoding Payload Payload
  | UndefinedLayer3Protocol
  | UnexpectedHTTPRequest Payload
  | UnhandledParseIP
  | UnhandledParseTCP
  | UnknownPortPair Port Port
  | UnhandledHTMLRequest TaggedHeaderRequest

instance Error StatsError where
  strMsg s = OtherError s

instance Show StatsError where
  show (HTTPError u r) = concat ["# Request to ", show u, " failed ", show r]
  show (IncompleteCapture wl cl) = "# Incomplete capture: " ++ show (wl, cl)
  show (OtherError s) = s
  show (UnacceptableEncoding h h1) = concat ["# Unacceptable encoding ", show h, " / ", show h1]
  show (UnexpectedHTTPRequest t) = "# Unknown/unexpected request " ++ show t
  show (UnknownPortPair sp dp) = "# Unknown port pair " ++ show (sp, dp)
  show (UnhandledHTMLRequest thr) = "# Don't know to parse " ++ show thr
  show FragmentationError = "# Unable to handle fragmentation at IP level"
  show MoreRequestsInConversation = "# One request only assumption failed"
  show UndefinedLayer3Protocol = "# Undefined layer 3 proto"
  show UnhandledParseIP = "# Unhandled parseIP case"
  show UnhandledParseTCP = "# Unhandled parseTCP case"

type StatsM = Either StatsError
