module Main where

import System.Directory
import System.FilePath.Posix
import Data.Time
import Data.Time.Format

-- * Defaults 

defaultFormat = "%y%m%d-%H%M%S"

-- * Types

type FormatString = String

-- * Adding meta information to file paths

addInfoToFilePath :: Adder -> Getter -> FilePath -> IO FilePath
addInfoToFilePath add get fp = add fp <$> get fp

addMTime :: Adder -> FormatString -> TimeLocale -> FilePath -> IO FilePath
addMTime add fmt loc = addInfoToFilePath add (getTimestamp fmt loc)

-- * Getters: Getting file information
type Getter = FilePath -> IO String

getTimestamp :: FormatString -> TimeLocale -> FilePath -> IO String
getTimestamp fmt loc fp = formatTime loc fmt <$> getModificationTime fp

-- * Adders: adding strings to file paths 
type Adder = FilePath -> String -> FilePath

prepend :: FilePath -> String -> FilePath
prepend f s = dir </> s ++ "-" ++ name
    where (dir, name) = splitFileName f

append :: FilePath -> String -> FilePath
append f s = name ++ "-" ++ s <.> ext
    where (name, ext) = splitExtension f

-- * Main

main :: IO ()
main = do
  putStrLn "hello world"
