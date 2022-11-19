module FTP.Commands
    ( Command(..)
    , parseCmd
    ) where

import Data.ByteString (ByteString)
import Data.Char       (toUpper)

import qualified Data.ByteString.Char8 as BS

data Command
    = User
    | Pass
    | Syst
    | Type
    | Feat
    | Pwd
    | Rmd
    | Mkd
    | Cwd
    | Pasv
    | Size
    | List
    | Retr
    | Stor
    | Dele
    | Rnfr
    | Rnto
    | Quit
    deriving (Eq, Ord, Show)

parseCmd :: ByteString -> Either ByteString (Command, ByteString)
parseCmd str = (,) <$> cmd <*> pure (BS.strip arg)
  where (strCmd, arg) = BS.break (== ' ') str
        cmd = case (BS.map toUpper $ BS.strip strCmd) of
            "USER" -> Right User
            "PASS" -> Right Pass
            "SYST" -> Right Syst
            "TYPE" -> Right Type
            "FEAT" -> Right Feat
            "PWD"  -> Right Pwd
            "RMD"  -> Right Rmd
            "MKD"  -> Right Mkd
            "CWD"  -> Right Cwd
            "PASV" -> Right Pasv
            "SIZE" -> Right Size
            "LIST" -> Right List
            "RETR" -> Right Retr
            "STOR" -> Right Stor
            "DELE" -> Right Dele
            "RNFR" -> Right Rnfr
            "RNTO" -> Right Rnto
            "QUIT" -> Right Quit
            _      -> Left $ "Unknown command " <> strCmd
