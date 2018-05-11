module Main where

import Options

-- * Main

main :: IO ()
main = do
    opts <- getOpts 
    putStrLn "Hello world!"
