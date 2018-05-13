{-# LANGUAGE DeriveDataTypeable #-}
module Options (Options(..), getOpts) where

import System.Console.CmdArgs.Implicit
import System.FilePath.Posix
import Data.Version
import Paths_timestamper

import Stamp

data Options = Options {
                source :: TimestampSource,
                format :: TimestampFormat,
                placement :: Placement,
                before :: String,
                after :: String,
                files :: [FilePath]
               } deriving (Show, Data, Typeable)

options = Options {
            source = TimeFileModified &= help "source of timestamp: TimeFileModified (default) or TimeNow",
            format = defaultFormat &= help ("timestamp format (default: " ++ defaultFormat ++ ")"),
            placement = Before &= help "timestamp placement: Before (default) or After filename",
            before = "" &= help "optional text before filename",
            after = "" &= help "optional text after filename",
            files = def &= typFile &= args 
          } &= program "timestamper"
            &= helpArg [name "h"]
            &= versionArg [name "v"]
            &= summary ("timestamper v" ++ showVersion version)
            &= details ["Add timestamps and optional text to one or more files.\n",
                        "The result will look like this:",
                        "   /path/to/[TIME-][TEXT-]filename[-TEXT][-TIME].ext"]

getOpts = cmdArgs options
