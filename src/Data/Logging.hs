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
                } deriving (Read, Eq)

instance Show LogMessage where
  show (LogMessage lmSource lmMessage lmTimestamp lmHiddenFlag lmLogLevel) = "[" ++ show lmLogLevel ++ "] " ++ show lmSource ++ ": " ++ lmMessage

instance Ord LogMessage where
  compare lm1 lm2 = case compare  (lmHiddenFlag lm2) (lmHiddenFlag lm1) of
                    EQ -> case compare (lmLogLevel lm1) (lmLogLevel lm2) of
                      EQ -> case compare (lmTimestamp lm1) (lmTimestamp lm2) of
                        ord -> ord
                      ord -> ord
                    ord -> ord


data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AnyInternal
                        | AnyExternal
                        | Any
                        | MatchAny [EventSourceMatcher]
                        | MatchAll [EventSourceMatcher]
                        deriving (Show, Read, Eq)


-- | Change log level operator
($=) :: LogMessage -> LogLevel -> LogMessage
($=) (LogMessage lmSource lmMessage lmTimestamp lmHiddenFlag lmLogLevel) changedLevel = LogMessage lmSource lmMessage lmTimestamp lmHiddenFlag changedLevel


-- | EventSource "combinator"
(@@) :: EventSource -> EventSource -> EventSource
(@@) e1@(Combined a1) e2@(Combined a2) = Combined (a1 ++ a2)
(@@) i1 e2@(Combined a2) = Combined ([i1] ++ a2)
(@@) e2@(Combined a2) i1 = Combined (a2 ++ [i1])
(@@) i1 i2 = (@@) (Combined [i1]) (Combined [i2])

-- | Matching EventSource with EventSourceMatcher operator
-- TODO: implement matching
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) (Exact ex) es = ex == es
(~~) (With es) (Combined xs) = elem es xs
(~~) (With es) _ = False
(~~) AnyInternal (Internal _ _ ) = True
(~~) AnyInternal (Combined (x:xs)) = (AnyInternal ~~ x) || (AnyInternal ~~ (Combined xs))
(~~) AnyInternal (Combined []) = False
(~~) AnyInternal _ = False
(~~) AnyExternal (External _ _ ) = True
(~~) AnyExternal (Combined (x:xs)) = (AnyExternal ~~ x) || (AnyExternal ~~ (Combined xs))
(~~) AnyExternal (Combined []) = False
(~~) AnyExternal _ = False
(~~) Any _ = True
(~~) (MatchAny (x:xs)) that = (x ~~ that) || (MatchAny xs ~~ that)
(~~) (MatchAny []) that = False
(~~) (MatchAll (x:xs)) that = (x ~~ that) && (MatchAll xs ~~ that)
(~~) (MatchAll []) that = True



-- | Specialized log list filter
-- TODO: implement filter function for logs with matchers, log level and hidden flag
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter  = undefined
