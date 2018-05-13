module Main where

import Control.Monad
import System.Console.CmdArgs.Verbosity
import System.Directory

import Options
import Stamp (stamp)

rename :: Bool -> FilePath -> FilePath -> IO ()
rename dry from to = do
    loud <- isLoud
    when (dry || loud) output
    where output = putStrLn (from ++ " -> " ++ to)

-- * Main

main :: IO ()
main = do
    (Options dry src fmt plc txtbef txtaft files) <- getOpts
    new <- mapM (stamp plc txtbef txtaft src fmt) files
    zipWithM_ (rename dry) files new
