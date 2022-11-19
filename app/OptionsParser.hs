module OptionsParser (parseOptions) where

import Data.ByteString            (ByteString)
import Options.Applicative        ( Parser, (<**>), execParser, optional, strOption
                                  , option, auto, value, helper, long, short, info
                                  , fullDesc
                                  )
import System.Environment         (getExecutablePath)
import System.Exit                (die)
import System.FilePath.ByteString (takeDirectory)

import FTP.Server (Settings(..))

import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.ByteString as BS

parseOptions :: IO Settings
parseOptions = do
    absPath <- fromStringUtf8 <$> getExecutablePath
    let curDir = takeDirectory absPath
    opts <- execParser $ info (optionsParser curDir) fullDesc
    case (settingsUser opts, settingsPassword opts) of
        (Just{}, Nothing) -> die "Password must be specified"
        (Nothing, Just{}) -> die "User must be specified"
        _                 -> pure opts

optionsParser :: ByteString -> Parser Settings
optionsParser curDir = options curDir <**> helper

options :: ByteString -> Parser Settings
options curDir = Settings
              <$> (optional . strOption)
                  ( long "user"
                 <> short 'u'
                  )
              <*> (optional . strOption)
                  ( long "password"
                 <> short 's'
                  )
              <*> option auto
                  ( long "port"
                 <> short 'p'
                 <> value 3000
                  )
              <*> strOption
                  ( long "directory"
                 <> short 'd'
                 <> value curDir
                  )

fromStringUtf8 :: String -> ByteString
fromStringUtf8 = BS.pack . UTF8.encode
