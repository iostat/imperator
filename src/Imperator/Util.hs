module Imperator.Util
  ( xor
  , readDateTime
  , showDateTime
  ) where

import           Control.Applicative   ((<|>))

import           Data.Time.Clock.POSIX
import           Data.Time.Format

-- not sure if theres an xor in the prelude and
-- i really dont want to pull in a package for this
xor :: Bool -> Bool -> Bool
True  `xor` b  = not b
False `xor` b  = b

-- Try to parse a string to a UNIX timestamp
readDateTime :: String -> Maybe Integer
readDateTime s = round . utcTimeToPOSIXSeconds  <$> tryParses
    where tryParses   =  parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) s
                     <|> parseTimeM True defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") s
-- and vice versa
showDateTime :: Integer -> String
showDateTime = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") . posixSecondsToUTCTime . realToFrac
