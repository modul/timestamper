{-# LANGUAGE DeriveDataTypeable #-}
module Stamp (
              -- * Defaults
              defaultFormat,
              -- * Types
              Placement(..),
              TimestampSource(..),
              TimestampFormat,
              -- * Adding text and timestamp to files
              stamp,
              getTimestamp,
              addToFilePath
             ) where

import System.Directory
import System.FilePath.Posix
import Data.Time
import Data.Time.Format
import Data.Data

-- * Defaults 
-- Default timestamp format.
defaultFormat :: TimestampFormat
defaultFormat = "%y%m%d-%H%M%S"
separationChar = "-"

-- * Utilities
format :: FormatTime t => String -> t -> String
format = formatTime defaultTimeLocale

utcToLocal :: IO (UTCTime -> ZonedTime)
utcToLocal = utcToZonedTime <$> getCurrentTimeZone

-- * Types

-- Format string for timestamps
type TimestampFormat = String

-- Source of timestamp
data TimestampSource = TimeFileModified | TimeNow deriving (Show, Data)

-- Text placement on file names
data Placement = Before | After deriving (Show, Data)

-- * Adding text to file paths

-- Adds text to a file path, before or after the file name.
addToFilePath :: Placement -> String -> FilePath -> FilePath
addToFilePath Before = prepend
addToFilePath After = append

prepend :: String -> FilePath -> FilePath
prepend "" f = f
prepend s f = dir </> s ++ separationChar ++ name
    where (dir, name) = splitFileName f

append :: String -> FilePath -> FilePath
append "" f = f
append s f = name ++ separationChar ++ s <.> ext
    where (name, ext) = splitExtension f

-- * Getting timestamps

-- Gets a timestamp as a formatted string from either the modification time of a file or the current time.
getTimestamp :: TimestampSource -> TimestampFormat -> FilePath -> IO String
getTimestamp TimeFileModified = getModTimestamp
getTimestamp TimeNow = getCurrentTimestamp

getModTimestamp :: TimestampFormat -> FilePath -> IO String
getModTimestamp fmt fp = format fmt <$> (utcToLocal <*> getModificationTime fp)

getCurrentTimestamp :: TimestampFormat -> FilePath -> IO String
getCurrentTimestamp fmt _ = format fmt <$> getZonedTime

-- * Stamp a file

-- Adds a timestamp and optional text to a file name. 
stamp :: 
      Placement         -- ^ where timestamp should be placed
    -> String            -- ^ text before the file name
    -> String            -- ^ text after the file name
    -> TimestampSource   -- ^ time source
    -> TimestampFormat   -- ^ timestamp format
    -> FilePath          -- ^ original file path
    -> IO FilePath       -- ^ modified file path

stamp place txtBefore txtAfter src fmt fp = do
    timestamp <- getTimestamp src fmt fp
    return . addToFilePath place timestamp . addToFilePath Before txtBefore . addToFilePath After txtAfter $ fp
