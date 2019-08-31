{-# LANGUAGE DeriveDataTypeable #-}
module Options (Options(..), getOpts) where

import System.Console.CmdArgs.Implicit
import System.FilePath.Posix
import Data.Version
import Paths_timestamper

import Stamp

data Options = Options {
                dry :: Bool,
                source :: TimestampSource,
                format :: TimestampFormat,
                placement :: Placement,
                textBefore :: String,
                textAfter :: String,
                files :: [FilePath]
               } deriving (Show, Data, Typeable)

options = Options {
            dry = False &= help "Don't do anything, just print actions",
            source = enum [
                TimeFileModified &= help "Use the modification time as a timestamp (default)" &= name "modified" &= name "m" &= explicit ,
                TimeNow &= help "Use the current time as a timestamp" &= name "now" &= name "n" &= explicit],
            format = defaultFormat &= help ("Timestamp format (default: " ++ defaultFormat ++ ")"),
            placement = enum [
                Before &= help "Put timestamp before the filename (default)", 
                After &= help "Put timestamp after the filename"],
            textBefore = "" &= help "Optional text before filename", 
            textAfter = "" &= help "Optional text after filename",
            files = def &= typFile &= args 
          } &= program "timestamper"
            &= helpArg [name "h"]
            &= help "Adds a timestamp and optional text to one or more file or directory names."
            &= summary ("timestamper v" ++ showVersion version)
            &= details ["Examples:",
                        "  timestamper -v -n basic.txt",
                        "  basic.txt -> 180513-1753-basic.txt",
                        "\n  timestamper -v -a --textbefore Test package.yaml",
                        "  package.yaml -> Test-package-180510-1512.yaml"]
            &= verbosity

getOpts = cmdArgs options
