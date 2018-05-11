module Main where

import Options

-- * Main

main :: IO ()
main = do
    print =<< getOpts 
