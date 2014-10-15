{-
Name: Myles Megyesi
Collaborators: none
Notes: I'm not in the class bro, I just like Haskell
-}

module CIS194.HW03
    ( compareMsgs
    , parse
    , parseMessage
    , sortMessages
    , validMessagesOnly
    , whatWentWrong
    ) where

import Data.List (sortBy)
import Prelude hiding (log)
import Text.Read (readMaybe)
import CIS194.Log
    ( LogMessage(LogMessage)
    , MaybeLogMessage(ValidLM, InvalidLM)
    , MessageType(Info, Warning, Error)
    )

splitOnFirstSpace :: String -> (String, String)
splitOnFirstSpace s = (beforeSpace, afterSpace)
    where isSpace = (== ' ')
          (beforeSpace, (_:afterSpace)) = break isSpace s

parseLeadingInt :: String -> (Maybe Int, String)
parseLeadingInt s = (readMaybe rawInt, rawMessage)
  where (rawInt, rawMessage) = (splitOnFirstSpace s)

parseLogWithTimestamp :: String -> String -> MessageType -> MaybeLogMessage
parseLogWithTimestamp wholeMessage messageWithoutLevel messageType =
    case maybeTime of
        Just timestamp -> ValidLM (LogMessage messageType timestamp rawMessage)
        _ -> InvalidLM wholeMessage
  where (maybeTime, rawMessage) = parseLeadingInt messageWithoutLevel

parseLogWithSeverityAndTimestamp :: String -> String -> (Int -> MessageType) -> MaybeLogMessage
parseLogWithSeverityAndTimestamp wholeMessage messageWithoutLevel messageType =
    case (maybeSeverity, maybeTime) of
        (Just severity, Just timestamp) -> ValidLM (LogMessage (messageType severity) timestamp rawMessage)
        _ -> InvalidLM wholeMessage
  where (maybeSeverity, messageWithoutSeverity) = parseLeadingInt messageWithoutLevel
        (maybeTime, rawMessage) = parseLeadingInt messageWithoutSeverity

parseMessage :: String -> MaybeLogMessage
parseMessage [] = InvalidLM []
parseMessage wholeMessage =
    case level of
        "I" -> parseLogWithTimestamp wholeMessage messageWithoutLevel Info
        "W" -> parseLogWithTimestamp wholeMessage messageWithoutLevel Warning
        "E" -> parseLogWithSeverityAndTimestamp wholeMessage messageWithoutLevel Error
        _ -> InvalidLM wholeMessage
  where (level, messageWithoutLevel) = (splitOnFirstSpace wholeMessage)

keepValidMessage :: MaybeLogMessage -> [LogMessage] -> [LogMessage]
keepValidMessage (ValidLM logMessage) validMessages = logMessage:validMessages
keepValidMessage (InvalidLM _) validMessages = validMessages

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly = foldr keepValidMessage []

parse :: String -> [LogMessage]
parse = validMessagesOnly . (map parseMessage) . lines

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ time1 _) (LogMessage _ time2 _) = compare time1 time2

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

compareSeverity :: (Int -> Bool) -> LogMessage -> Bool
compareSeverity comp (LogMessage (Error serverity) _ _) = comp serverity
compareSeverity _ _ = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ message) = message

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map extractMessage) . filter (compareSeverity (>= 50))
