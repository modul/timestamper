{-# LANGUAGE DeriveDataTypeable #-}
module Options (Options(..), getOpts) where

import System.Console.CmdArgs.Implicit
import qualified Filename as FN
import System.FilePath.Posix

import Data.Version
--import Paths_timestamper

data Options = Options {
                addTimestamp :: Bool,
                addString :: Bool,              
                prepend :: Bool,
                append :: Bool,
                timestampSource :: FN.TimestampSource,
                timestampFormat :: FN.FormatString,
                string :: String,
                files :: [FilePath]
               } deriving (Show, Data, Typeable)

options = Options {
            addTimestamp = True &= help "add timestamp to given file(s) (default: True)",
            addString = False &= help "add a constant text to given file(s) (default: False)",
            prepend = True &= help "prepend to filename (default: True)",
            append = False &= help "append to filename (default: False)",
            timestampSource = FN.TimeFileModified &= help "source for timestamp: TimeFileModified (default) or CurrentTime",
            timestampFormat = FN.defaultFormat &= help ("timestamp format (default: " ++ FN.defaultFormat ++ ")"),
            string = "" &= help "constant text to add to filename (default: '')",
            files = def &= typFile &= args
          } &= program "timestamper"
--            &== summary ("timestamper v" ++ showVersion version)

getOpts = cmdArgs options
