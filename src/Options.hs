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
                before :: String,
                after :: String,
                files :: [FilePath]
               } deriving (Show, Data, Typeable)

options = Options {
            dry = False &= help "don't do anything, just print actions" &= name "n",
            source = TimeFileModified &= help "source of timestamp: TimeFileModified (default) or TimeNow",
            format = defaultFormat &= help ("timestamp format (default: " ++ defaultFormat ++ ")"),
            placement = Before &= help "timestamp placement: Before (default) or After filename",
            before = "" &= help "optional text before filename",
            after = "" &= help "optional text after filename",
            files = def &= typFile &= args 
          } &= program "timestamper"
            &= helpArg [name "h"]
            &= help "Add a timestamp (file modified or current time) and optional text to one or more file paths. Works on files and directories."
            &= summary ("timestamper v" ++ showVersion version)
            &= details ["Examples:",
                        "  timestamper -v -s TimeNow basic.txt",
                        "  basic.txt -> 180513-1753-basic.txt",
                        "\n  timestamper -v -p After -b Test package.yaml",
                        "  package.yaml -> Test-package-180510-1512.yaml"]
            &= verbosity

getOpts = cmdArgs options
