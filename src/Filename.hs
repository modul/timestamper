{-# LANGUAGE DeriveDataTypeable #-}
module Filename where

import System.Directory
import System.FilePath.Posix
import Data.Time
import Data.Time.Format
import Data.Data

-- * Defaults 
defaultFormat = "%y%m%d-%H%M%S"
separationChar = "-"

-- * Utilities
format :: FormatTime t => String -> t -> String
format = formatTime defaultTimeLocale

utcToLocal :: IO (UTCTime -> ZonedTime)
utcToLocal = utcToZonedTime <$> getCurrentTimeZone

-- * Types
type FormatString = String
data TimestampSource = TimeFileModified | CurrentTime deriving (Show, Data)

-- * Adding meta information to file paths
type AddMeta = Adder -> FilePath -> IO FilePath

addString :: String -> AddMeta
addString s = addInfoToFilePath (getConst s)

addTimestamp :: TimestampSource -> FormatString -> AddMeta
addTimestamp src fmt = addInfoToFilePath $ (get src) fmt
    where get TimeFileModified = getModTimestamp 
          get CurrentTime = getCurrentTimestamp

addModTime :: FormatString -> AddMeta
addModTime = addTimestamp TimeFileModified

addCurTime :: FormatString -> AddMeta
addCurTime = addTimestamp CurrentTime 

addInfoToFilePath :: Getter -> AddMeta
addInfoToFilePath get add fp = add fp <$> get fp

-- * Getters: Getting file information
type Getter = FilePath -> IO String

getConst :: String -> FilePath -> IO String
getConst s _ = return s

getModTimestamp :: FormatString -> FilePath -> IO String
getModTimestamp fmt fp = format fmt <$> (utcToLocal <*> getModificationTime fp)

getCurrentTimestamp :: FormatString -> FilePath -> IO String
getCurrentTimestamp fmt _ = format fmt <$> getCurrentTime

-- * Adders: adding strings to file paths 
type Adder = FilePath -> String -> FilePath

prepend :: FilePath -> String -> FilePath
prepend f s = dir </> s ++ separationChar ++ name
    where (dir, name) = splitFileName f

append :: FilePath -> String -> FilePath
append f s = name ++ separationChar ++ s <.> ext
    where (name, ext) = splitExtension f


