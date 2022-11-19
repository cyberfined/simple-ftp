{-# LANGUAGE ScopedTypeVariables #-}

module FTP.Server
    ( Settings(..)
    , runServer
    ) where

import           Control.Concurrent         (forkIO)
import           Control.Concurrent.MVar    (MVar, modifyMVar, modifyMVar_,
                                             newEmptyMVar, newMVar, putMVar,
                                             readMVar, takeMVar, tryTakeMVar)
import           Control.Monad              (forM_, void)
import           Control.Monad.Catch        (MonadCatch (..), MonadThrow (..),
                                             SomeException, catch, tryJust)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (MonadReader (..), ReaderT (..),
                                             asks)
import           Data.Bits                  (shiftR, (.&.))
import           Data.ByteString            (ByteString)
import           Data.Fixed                 (Fixed (..))
import           Data.IntSet                (IntSet)
import           Data.IORef                 (IORef, newIORef, readIORef,
                                             writeIORef)
import           Data.Time                  (defaultTimeLocale, formatTime,
                                             secondsToNominalDiffTime)
import           Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import           Data.Word                  (Word16, Word8)
import           GHC.IO.Exception           (IOErrorType (..), IOException (..))
import           Network.Simple.TCP         (HostPreference (..), Socket,
                                             bindSock, closeSock, listenSock,
                                             recv, send, serve)
import           Network.Socket             (SockAddr (..), accept,
                                             getSocketName, hostAddressToTuple)
import           Prelude                    hiding (read)
import           System.Directory           (createDirectory,
                                             doesDirectoryExist, doesFileExist,
                                             doesPathExist, listDirectory,
                                             removeDirectory, removeFile,
                                             renamePath)
import           System.FilePath.ByteString (equalFilePath, takeDirectory,
                                             (</>))
import           System.IO                  (Handle, IOMode (..), openFile,
                                             withFile)
import           System.Log.FastLogger      (LogStr, LogType' (..),
                                             TimedFastLogger, ToLogStr (..),
                                             newTimedFastLogger)
import           System.Log.FastLogger.Date (newTimeCache, simpleTimeFormat')
import           System.Posix.Types         (FileMode)
import           System.PosixCompat.Files   (FileStatus, fileGroup, fileMode,
                                             fileOwner, fileSize, getFileStatus,
                                             groupExecuteMode, groupReadMode,
                                             groupWriteMode, isDirectory,
                                             linkCount, modificationTime,
                                             otherExecuteMode, otherReadMode,
                                             otherWriteMode, ownerExecuteMode,
                                             ownerReadMode, ownerWriteMode)

import           FTP.Commands

import qualified Codec.Binary.UTF8.String   as UTF8
import qualified Data.ByteString            as BSWord
import qualified Data.ByteString.Char8      as BS
import qualified Data.IntSet                as IntSet

data Settings = Settings
    { settingsUser      :: !(Maybe ByteString)
    , settingsPassword  :: !(Maybe ByteString)
    , settingsPort      :: !Int
    , settingsDirectory :: !ByteString
    }

runServer :: Settings -> IO ()
runServer settings@Settings{..} = do
    portMVar <- newMVar 1024
    usedPortsMVar <- newMVar IntSet.empty
    timeCache <- newTimeCache simpleTimeFormat'
    (logger, _) <- newTimedFastLogger timeCache (LogStderr 1024)
    currentPortRef <- newIORef 0
    serve HostIPv4 (show settingsPort) $ \(sock, _) -> do
        ipAddr <- getSocketName sock >>= \case
            SockAddrInet _ host -> pure $ hostAddressToTuple host
            _                   -> error "Only ipv4 protocol is supported"
        dirRef <- newIORef settingsDirectory
        relativeDirRef <- newIORef "/"
        fileConnMVar <- newEmptyMVar
        userRef <- newIORef Nothing
        authoredRef <- newIORef False
        isModeSetRef <- newIORef False
        renameFromRef <- newIORef Nothing
        let ctx = ClientContext
                { clientSettings    = settings
                , clientCmdConn     = sock
                , clientFileConn    = fileConnMVar
                , clientCurDir      = dirRef
                , clientRelativeDir = relativeDirRef
                , clientUser        = userRef
                , clientAuthored    = authoredRef
                , clientLastPort    = portMVar
                , clientIpAddr      = ipAddr
                , clientUsedPorts   = usedPortsMVar
                , clientIsModeSet   = isModeSetRef
                , clientLogger      = logger
                , clientCurrentPort = currentPortRef
                , clientRenameFrom  = renameFromRef
                }
        runReaderT (runServerM handleConn) ctx

newtype ServerM a = ServerM { runServerM :: ReaderT ClientContext IO a }
    deriving newtype ( Functor
                     , Applicative
                     , Monad
                     , MonadIO
                     , MonadReader ClientContext
                     , MonadThrow
                     , MonadCatch
                     )

data ClientContext = ClientContext
    { clientSettings    :: !Settings
    , clientCmdConn     :: !Socket
    , clientFileConn    :: !(MVar Socket)
    , clientCurDir      :: !(IORef ByteString)
    , clientRelativeDir :: !(IORef ByteString)
    , clientUser        :: !(IORef (Maybe ByteString))
    , clientAuthored    :: !(IORef Bool)
    , clientIpAddr      :: !(Word8, Word8, Word8, Word8)
    , clientLastPort    :: !(MVar Word16)
    , clientUsedPorts   :: !(MVar IntSet)
    , clientIsModeSet   :: !(IORef Bool)
    , clientLogger      :: !TimedFastLogger
    , clientCurrentPort :: !(IORef Int)
    , clientRenameFrom  :: !(IORef (Maybe ByteString))
    }

handleConn :: ServerM ()
handleConn = do
    sendResponse 220 "Welcome to server."
    loop
  where loop = do
            cmdConn <- asks clientCmdConn
            recv cmdConn 4096 >>= \case
              Nothing     -> pure ()
              Just strCmd -> do
                  writeLog (toLogStr strCmd)
                  case parseCmd strCmd of
                      Left err         -> do
                          sendResponse 500 err
                          loop
                      Right (cmd, arg) -> case cmd of
                          Quit -> pure ()
                          _    -> do
                              catch (handleCmd cmd arg) $ \(e :: SomeException) -> do
                                  writeLog (toLogStr $ show e)
                                  sendResponse 550 "Something went wrong."
                              loop

writeLog :: LogStr -> ServerM ()
writeLog msg = do
    logger <- asks clientLogger
    liftIO $ logger format
  where format time = toLogStr time <> " " <> msg

handleCmd :: Command -> ByteString -> ServerM ()
handleCmd User arg = do
    userRef <- asks clientUser
    liftIO $ writeIORef userRef (Just arg)
    sendResponse 331 ("Password required for " <> arg <> ".")
handleCmd Pass authPassword = do
    mAuthUser <- asks clientUser >>= liftIO . readIORef
    case mAuthUser of
        Nothing -> sendResponse 503 "Login with USER first."
        Just authUser -> do
            settings <- asks clientSettings
            authoredRef <- asks clientAuthored
            let loggedResponse = do
                    liftIO $ writeIORef authoredRef True
                    sendResponse 230 "User logged in."
            case (settingsUser settings, settingsPassword settings) of
                (Just user, Just password)
                  | user == authUser
                  , authPassword == password
                  -> loggedResponse
                  | otherwise -> sendResponse 530 "User cannot log in."
                _ -> loggedResponse
handleCmd Syst _ = sendResponse 215 "UNIX TYPE: L8."
handleCmd Type _ = sendResponse 200 "Type set to I."
handleCmd Feat _ = do
    cmdConn <- asks clientCmdConn
    send cmdConn "211-Features:\n SIZE\n UTF8\n211 End\n"
handleCmd Pwd _ = authCmd $ do
    relativeDirRef <- asks clientRelativeDir
    relativeDir <- liftIO $ readIORef relativeDirRef
    sendResponse 257 $ "\"" <> relativeDir <> "\" is current directory."
handleCmd Rmd dir = authCmd $ do
    (rmDir, _) <- concatFilePathM dir
    let strRmDir = toStringUtf8 rmDir
    liftIO (doesDirectoryExist strRmDir) >>= \case
        False -> sendResponse 550 "The system cannot find the file specified."
        True  -> do
            let handler :: IOException -> Maybe Bool
                handler e = Just $ ioe_type e == UnsatisfiedConstraints

            liftIO (tryJust handler $ removeDirectory strRmDir) >>= \case
                Left True  -> sendResponse 550 "Directory not empty."
                Left False -> sendResponse 550 "Something went wrong."
                Right{}    -> sendResponse 250 "Directory was deleted."
handleCmd Mkd dir = authCmd $ do
    (mkDir, _) <- concatFilePathM dir
    liftIO $ createDirectory (toStringUtf8 mkDir)
    sendResponse 250 "Directory was created"
handleCmd Cwd dir = authCmd $ do
    curDirRef <- asks clientCurDir
    relativeDirRef <- asks clientRelativeDir
    curDir <- liftIO $ readIORef curDirRef
    relativeDir <- liftIO $ readIORef relativeDirRef
    rootDir <- asks (settingsDirectory . clientSettings)
    let (newCurDir, newRelativeDir) = concatFilePath rootDir curDir relativeDir dir
    doesExist <- liftIO $ doesDirectoryExist (toStringUtf8 newCurDir)
    if doesExist
       then do
           liftIO $ writeIORef curDirRef newCurDir
           liftIO $ writeIORef relativeDirRef newRelativeDir
           sendResponse 250 "CWD command successful."
       else sendResponse 550 "The system cannot find the file specified."
handleCmd Pasv _ = authCmd $ getFreePort >>= \case
    Nothing   -> sendResponse 425 "All ports are used."
    Just port -> do
        currentPortRef <- asks clientCurrentPort
        liftIO $ writeIORef currentPortRef (fromIntegral port)
        fileConnMVar <- asks clientFileConn
        liftIO $ tryTakeMVar fileConnMVar >>= \case
            Nothing       -> pure ()
            Just fileConn -> closeSock fileConn
        (ip1, ip2, ip3, ip4) <- asks clientIpAddr
        let port1 = (port .&. 0xff00) `shiftR` 8
        let port2 = port .&. 0xff
        let pasvAddr = BS.intercalate "," [ BS.pack $ show ip1
                                          , BS.pack $ show ip2
                                          , BS.pack $ show ip3
                                          , BS.pack $ show ip4
                                          , BS.pack $ show port1
                                          , BS.pack $ show port2
                                          ]
        void $ liftIO $ forkIO $ do
            (sock, _) <- liftIO $ bindSock HostIPv4 (show port)
            listenSock sock 2048
            (fileConn, _) <- accept sock
            putMVar fileConnMVar fileConn
        isModeSetRef <- asks clientIsModeSet
        liftIO $ writeIORef isModeSetRef True
        sendResponse 227 $ "Entering Passive Mode (" <> pasvAddr <> ")"
handleCmd Size file = authCmd $ do
    curDirRef <- asks clientCurDir
    curDir <- liftIO $ readIORef curDirRef
    let absFilePath = curDir </> file
    doesExist <- liftIO $ doesFileExist (toStringUtf8 absFilePath)
    if doesExist
       then do
           fStat <- liftIO $ getFileStatus (toStringUtf8 absFilePath)
           let size = fileSize fStat
           sendResponse 213 (BS.pack $ show size)
       else sendResponse 550 "The system cannot find the file specified."
handleCmd List args = fileConnCmd $ do
    let dir = getListDirectory args
    (listDir, _) <- concatFilePathM dir
    let strListDir = toStringUtf8 listDir
    doesExist <- liftIO $ doesDirectoryExist strListDir
    if not doesExist
       then sendResponse 550 "The system cannot find the file specified."
       else do
           files <- liftIO $ listDirectory strListDir
           sendResponse 150 "Opening BINARY mode data connection."
           fileConnMVar <- asks clientFileConn
           fileConn <- liftIO $ readMVar fileConnMVar
           forM_ files $ \file -> liftIO $ do
               let fileName = fromStringUtf8 file
               let filePath = listDir </> fileName
               fStat <- getFileStatus (toStringUtf8 filePath)
               let line = formatFileStat fileName fStat
               send fileConn (line <> "\n")
           closeFileConn
           sendResponse 226 "Transfer complete."
handleCmd Retr file = fileConnCmd $ getFile file >>= \case
    Nothing             -> sendResponse 550 "The system cannot find the file specified."
    Just (handle, size) -> do
        let openMsg =  "Opening BINARY mode data connection for "
                    <> file <> " (" <> BS.pack (show size) <> " bytes)"
        sendResponse 150 openMsg
        sendFile handle
        closeFileConn
        sendResponse 226 "Transfer complete."
handleCmd Stor file = fileConnCmd $ do
    (newFile, _) <- concatFilePathM file
    let upDirectory = takeDirectory newFile
    doesExist <- liftIO $ doesDirectoryExist (toStringUtf8 upDirectory)
    if doesExist
       then do
           sendResponse 150 "Opening BINARY mode data connection."
           retrieveFile newFile
           sendResponse 226 "Transfer complete."
       else do
           sendResponse 550 "Directory does not exist."
handleCmd Dele file = authCmd $ do
    (rmFile, _) <- concatFilePathM file
    let strRmFile = toStringUtf8 rmFile
    doesExist <- liftIO $ doesFileExist strRmFile
    if doesExist
       then do
           liftIO $ removeFile strRmFile
           sendResponse 250 "File was deleted."
       else sendResponse 550 "The system cannot find the file specified."
handleCmd Rnfr file = authCmd $ do
    (rnFile, _) <- concatFilePathM file
    doesExist <- liftIO $ doesPathExist (toStringUtf8 rnFile)
    if doesExist
       then do
           renameFromRef <- asks clientRenameFrom
           liftIO $ writeIORef renameFromRef $ Just rnFile
           sendResponse 350 "Waiting for RNTO command."
       else sendResponse 550 "The system cannot find the file specified."
handleCmd Rnto renameTo = authCmd $ do
    renameFromRef <- asks clientRenameFrom
    liftIO (readIORef renameFromRef) >>= \case
        Nothing         -> sendResponse 425 "Use RNFR first."
        Just renameFrom -> do
            liftIO $ renamePath (toStringUtf8 renameFrom) (toStringUtf8 renameTo)
            liftIO $ writeIORef renameFromRef Nothing
            sendResponse 250 "File was renamed."
handleCmd Quit _ = pure ()

authCmd :: ServerM () -> ServerM ()
authCmd handler = do
    authoredRef <- asks clientAuthored
    isAuthored <- liftIO $ readIORef authoredRef
    if not isAuthored
       then sendResponse 530 "Please login with USER and PASS."
       else handler

fileConnCmd :: ServerM () -> ServerM ()
fileConnCmd handler = authCmd $ do
    isModeSetRef <- asks clientIsModeSet
    liftIO (readIORef isModeSetRef) >>= \case
        False -> sendResponse 425 "Use PORT or PASV first."
        True  -> handler

sendResponse :: Int -> ByteString -> ServerM ()
sendResponse code msg = do
    cmdConn <- asks clientCmdConn
    send cmdConn msgWithCode
  where codeStr = BS.pack (show code)
        msgWithCode = codeStr <> " " <> msg <> "\r\n"

getFreePort :: ServerM (Maybe Word16)
getFreePort = do
    lastPortMVar <- asks clientLastPort
    usedPortsMVar <- asks clientUsedPorts
    loop lastPortMVar usedPortsMVar 0
  where loop :: MVar Word16 -> MVar IntSet -> Int -> ServerM (Maybe Word16)
        loop lastPortMVar usedPortsMVar step
          | step < maxSteps = do
              port <- liftIO $ modifyMVar lastPortMVar $ \lastPort ->
                  pure (lastPort + 1, lastPort)
              isFree <- liftIO $ modifyMVar usedPortsMVar $ \usedPorts -> do
                  if IntSet.member (fromIntegral port) usedPorts
                     then pure (usedPorts, False)
                     else pure (IntSet.insert (fromIntegral port) usedPorts, True)
              if isFree
                 then pure (Just port)
                 else loop lastPortMVar usedPortsMVar (step + 1)
          | otherwise = pure Nothing
          where maxSteps = 100

sendFile :: Handle -> ServerM ()
sendFile handle = do
    fileConnMVar <- asks clientFileConn
    fileConn <- liftIO $ readMVar fileConnMVar
    loop fileConn
  where loop fileConn = do
            chunk <- liftIO $ BS.hGet handle 4096
            if chunk == BS.empty
               then pure ()
               else send fileConn chunk >> loop fileConn

retrieveFile :: ByteString -> ServerM ()
retrieveFile path = do
    fileConnMVar <- asks clientFileConn
    fileConn <- liftIO $ readMVar fileConnMVar
    liftIO $ withFile (toStringUtf8 path) WriteMode (loop fileConn)
  where loop fileConn handle = recv fileConn 4096 >>= \case
            Nothing    -> pure ()
            Just chunk -> BS.hPut handle chunk >> loop fileConn handle


getFile :: ByteString -> ServerM (Maybe (Handle, Int))
getFile file = do
    (filePathBs, _) <- concatFilePathM file
    let filePath = toStringUtf8 filePathBs
    doesExist <- liftIO $ doesFileExist filePath
    if doesExist
       then liftIO $ do
           fStat <- getFileStatus filePath
           handle <- openFile filePath ReadMode
           pure $ Just (handle, fromIntegral $ fileSize fStat)
       else pure Nothing

closeFileConn :: ServerM ()
closeFileConn = do
    currentPortRef <- asks clientCurrentPort
    currentPort <- liftIO $ readIORef currentPortRef
    usedPortsMVar <- asks clientUsedPorts
    liftIO $ modifyMVar_ usedPortsMVar (pure . IntSet.delete currentPort)
    fileConnMVar <- asks clientFileConn
    fileConn <- liftIO $ takeMVar fileConnMVar
    closeSock fileConn
    isModeSetRef <- asks clientIsModeSet
    liftIO $ writeIORef isModeSetRef False

formatFileStat :: ByteString -> FileStatus -> ByteString
formatFileStat fileName stat =
    BS.intercalate "\t" [ modeStr
                        , BS.pack $ show $ linkCount stat
                        , BS.pack $ show $ fileOwner stat
                        , BS.pack $ show $ fileGroup stat
                        , BS.pack $ show $ fileSize stat
                        , formattedTime
                        , fileName
                        ]
  where modeStr =  formatDirectory
                <> formatReadWriteExec ownerReadMode ownerWriteMode ownerExecuteMode
                <> formatReadWriteExec groupReadMode groupWriteMode groupExecuteMode
                <> formatReadWriteExec otherReadMode otherWriteMode otherExecuteMode

        formattedTime = BS.pack $ formatTime defaultTimeLocale "%b %d %Y" modTime
        modTime = posixSecondsToUTCTime $ secondsToNominalDiffTime $
            MkFixed $ ((fromIntegral $ fromEnum $ modificationTime stat) * 1000000000000)

        mode = fileMode stat

        formatDirectory = if isDirectory stat then "d" else "-"

        formatReadWriteExec :: FileMode -> FileMode -> FileMode -> ByteString
        formatReadWriteExec read write exec = r <> w <> x
          where r = if mode .&. read /= 0 then "r" else "-"
                w = if mode .&. write /= 0 then "w" else "-"
                x = if mode .&. exec /= 0 then "x" else "-"

getListDirectory :: ByteString -> ByteString
getListDirectory arg = BS.unwords $ loop args
  where args = BS.words arg

        loop :: [ByteString] -> [ByteString]
        loop toks@(x:xs)
          | BS.isPrefixOf "-" x = loop xs
          | otherwise           = toks
        loop []                 = []

concatFilePathM :: ByteString -> ServerM (ByteString, ByteString)
concatFilePathM path = do
    curDirRef <- asks clientCurDir
    relativeDirRef <- asks clientRelativeDir
    curDir <- liftIO $ readIORef curDirRef
    relativeDir <- liftIO $ readIORef relativeDirRef
    rootDir <- asks (settingsDirectory . clientSettings)
    pure $ concatFilePath rootDir curDir relativeDir path

concatFilePath :: ByteString
               -> ByteString
               -> ByteString
               -> ByteString
               -> (ByteString, ByteString)
concatFilePath rootDir absDir relDir path
  | BS.isPrefixOf "/" path  =
      concatFilePath rootDir rootDir "/" (BS.drop 1 path)
  | BS.isPrefixOf ".." path =
      let upDir = takeDirectory absDir
          upRelDir = takeDirectory relDir
          droppedPath = BS.drop 2 path
          nextPath = if BS.isPrefixOf "/" droppedPath
                        then BS.drop 1 droppedPath
                        else droppedPath
      in if equalFilePath absDir rootDir
            then concatFilePath rootDir rootDir "/" nextPath
            else concatFilePath rootDir upDir upRelDir nextPath
  | BS.isPrefixOf "./" path =
      concatFilePath rootDir absDir relDir (BS.drop 2 path)
  | BS.isPrefixOf "." path =
      concatFilePath rootDir absDir relDir (BS.drop 1 path)
  | otherwise =
      let (pathSegment, restPath) = (BS.drop 1) <$> BS.break (== '/') path
          newAbsDir = absDir </> pathSegment
          newRelDir = relDir </> pathSegment
      in if restPath == ""
            then (newAbsDir, newRelDir)
            else concatFilePath rootDir newAbsDir newRelDir restPath

toStringUtf8 :: ByteString -> String
toStringUtf8 = UTF8.decode . BSWord.unpack

fromStringUtf8 :: String -> ByteString
fromStringUtf8 = BSWord.pack . UTF8.encode
