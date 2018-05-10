module Main where

import System.Directory
import System.FilePath.Posix
import Data.Time
import Data.Time.Format

-- * Defaults 

defaultFormat = "%y%m%d-%H%M%S"
separationChar = "-"

-- * Types

type FormatString = String

-- * Adding meta information to file paths
type AddMeta = Adder -> FilePath -> IO FilePath

addString :: String -> AddMeta
addString s = addInfoToFilePath (getConst s)

addMTime :: FormatString -> TimeLocale -> AddMeta
addMTime fmt loc = addInfoToFilePath (getTimestamp fmt loc)

addInfoToFilePath :: Getter -> AddMeta
addInfoToFilePath get add fp = add fp <$> get fp

-- * Getters: Getting file information
type Getter = FilePath -> IO String

getConst :: String -> FilePath -> IO String
getConst s _ = return s

getTimestamp :: FormatString -> TimeLocale -> FilePath -> IO String
getTimestamp fmt loc fp = formatTime loc fmt <$> getModificationTime fp

-- * Adders: adding strings to file paths 
type Adder = FilePath -> String -> FilePath

prepend :: FilePath -> String -> FilePath
prepend f s = dir </> s ++ separationChar ++ name
    where (dir, name) = splitFileName f

append :: FilePath -> String -> FilePath
append f s = name ++ separationChar ++ s <.> ext
    where (name, ext) = splitExtension f

-- * Main

main :: IO ()
main = do
  putStrLn "hello world"
