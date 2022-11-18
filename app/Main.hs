module Main (main) where

import FTP.Server (Settings(..), runServer)

main :: IO ()
main = runServer settings
  where settings = Settings Nothing Nothing 3000 dir
        dir = "/home/cyberfined/Documents/simple-ftp"
