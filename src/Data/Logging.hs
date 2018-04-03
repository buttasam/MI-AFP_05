module Data.Logging where

import Data.Time.Clock
import Data.List

data LogLevel = Debug | Info | Warning | Error | Fatal
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

data EventSource = Internal { iesComponent   :: String
                            , iesCallID      :: String }
                 | External { eesURI         :: String
                            , eesDescription :: String }
                 | Combined [EventSource]
                 | Unknown
                 deriving (Read, Eq, Ord)
                 -- TODO: remove Ord here after implementing Ord for LogMessage
instance Show EventSource where
    show (Internal iesComponent iesCallID) = "Internal[" ++ iesComponent ++ "]"
    show (External eesURI eesDescription) = "External[" ++ eesURI ++ "]"
    show (Combined srcArr) = "Combined" ++ show srcArr
    show Unknown = "Unknown"

data LogMessage = LogMessage
                { lmSource     :: EventSource
                , lmMessage    :: String
                , lmTimestamp  :: UTCTime
                , lmHiddenFlag :: Bool
                , lmLogLevel   :: LogLevel
                } deriving (Read, Eq, Ord)
                -- TODO: custom instance of Show and Ord (hidden, timestamp, logLevel)

data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AnyInternal
                        | AnyExternal
                        | Any
                        | MatchAny [EventSourceMatcher]
                        | MatchAll [EventSourceMatcher]
                        deriving (Show, Read, Eq)

instance Show LogMessage where
  show (LogMessage lmSource lmMessage lmTimestamp lmHiddenFlag lmLogLevel) = "[" ++ show lmLogLevel ++ "] " ++ show lmSource ++ ": " ++ lmMessage

-- | Change log level operator
-- TODO: implement operator which changes LogLevel of LogMessage
($=) :: LogMessage -> LogLevel -> LogMessage
($=) = undefined


-- | EventSource "combinator"
-- TODO: implement operator which combines two EventSources (just 1 level for Combined, see tests)
(@@) :: EventSource -> EventSource -> EventSource
(@@) = undefined

-- | Matching EventSource with EventSourceMatcher operator
-- TODO: implement matching
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) = undefined

-- | Specialized log list filter
-- TODO: implement filter function for logs with matchers, log level and hidden flag
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter  = undefined
