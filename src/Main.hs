module Main where

import Options
import Stamp (stamp)

rename :: Bool -> FilePath -> FilePath -> IO ()
rename dry from to = if dry then output else return ()
    where output = putStrLn (from ++ " -> " ++ to)

-- * Main

main :: IO ()
main = do
    (Options dry src fmt plc txtbef txtaft files) <- getOpts
    new <- mapM (stamp plc txtbef txtaft src fmt) files
    zipWithM_ (rename dry) files new
