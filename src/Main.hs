module Main where

import Options
import Stamp (stamp)

-- * Main

main :: IO ()
main = do
    (Options src fmt plc txtbef txtaft files) <- getOpts
    print =<< mapM (stamp plc txtbef txtaft src fmt) files
