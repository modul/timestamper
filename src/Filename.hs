module Filename where

import System.Directory
import System.FilePath.Posix
import Data.Time
import Data.Time.Format

-- * Defaults 

defaultFormat = "%y%m%d-%H%M%S"
separationChar = "-"

-- * Types

type FormatString = String
data TimestampSource = TimeFileModified | CurrentTime deriving Show

-- * Adding meta information to file paths
type AddMeta = Adder -> FilePath -> IO FilePath

addString :: String -> AddMeta
addString s = addInfoToFilePath (getConst s)

addTimestamp :: TimestampSource -> FormatString -> TimeLocale -> AddMeta
addTimestamp src fmt loc = addInfoToFilePath $ uncurry (get src) (fmt, loc)
    where get TimeFileModified = getModTimestamp 
          get CurrentTime = getCurrentTimestamp

addModTime :: FormatString -> TimeLocale -> AddMeta
addModTime = addTimestamp TimeFileModified

addCurTime :: FormatString -> TimeLocale -> AddMeta
addCurTime = addTimestamp CurrentTime 

addInfoToFilePath :: Getter -> AddMeta
addInfoToFilePath get add fp = add fp <$> get fp

-- * Getters: Getting file information
type Getter = FilePath -> IO String

getConst :: String -> FilePath -> IO String
getConst s _ = return s

getModTimestamp :: FormatString -> TimeLocale -> FilePath -> IO String
getModTimestamp fmt loc fp = formatTime loc fmt <$> getModificationTime fp

getCurrentTimestamp :: FormatString -> TimeLocale -> FilePath -> IO String
getCurrentTimestamp fmt loc _ = formatTime loc fmt <$> getCurrentTime 

-- * Adders: adding strings to file paths 
type Adder = FilePath -> String -> FilePath

prepend :: FilePath -> String -> FilePath
prepend f s = dir </> s ++ separationChar ++ name
    where (dir, name) = splitFileName f

append :: FilePath -> String -> FilePath
append f s = name ++ separationChar ++ s <.> ext
    where (name, ext) = splitExtension f


