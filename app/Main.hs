module Main (main) where

import FTP.Server    (runServer)
import OptionsParser (parseOptions)

main :: IO ()
main = do
    settings <- parseOptions
    runServer settings
