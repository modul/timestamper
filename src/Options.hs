{-# LANGUAGE DeriveDataTypeable #-}
module Options (Options(..), getOpts) where

import System.Console.CmdArgs.Implicit
import Paths_addname
import Data.Version

data Options = Options {
               } deriving (Show, Data, Typeable)

options = Options {
{-            width  = 80 &= help "Display width (default: 80)",
            height = 40 &= help "Display height (default: 40)",
            fps    = 30 &= help "Display speed (default: 30)"
-}            
          } &= program "addname"
            &= summary ("addname v" ++ showVersion version)

getOpts = cmdArgs options
