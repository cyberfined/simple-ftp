module FTP.Commands
    ( Command(..)
    , parseCmd
    ) where

import Data.ByteString (ByteString)

import qualified Data.ByteString.Char8 as BS

data Command
    = User
    | Pass
    | Syst
    | Type
    | Feat
    | Pwd
    | Cwd
    | Pasv
    | Size
    | List
    | Retr
    | Quit
    deriving (Eq, Ord, Show)

parseCmd :: ByteString -> Either ByteString (Command, ByteString)
parseCmd str = (,) <$> cmd <*> pure (BS.strip arg)
  where (strCmd, arg) = BS.break (== ' ') str
        cmd = case (BS.strip strCmd) of
            "USER" -> Right User
            "PASS" -> Right Pass
            "SYST" -> Right Syst
            "TYPE" -> Right Type
            "FEAT" -> Right Feat
            "PWD"  -> Right Pwd
            "CWD"  -> Right Cwd
            "PASV" -> Right Pasv
            "SIZE" -> Right Size
            "LIST" -> Right List
            "RETR" -> Right Retr
            "QUIT" -> Right Quit
            _      -> Left $ "Unknown command " <> strCmd
