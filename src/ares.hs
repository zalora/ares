{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Bits ((.|.))
import Data.List (intercalate)
import Data.List.Unique (sortUniq)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Network.Socket
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import System.Directory
import System.Environment (setEnv)
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Posix.Files (groupReadMode, groupWriteMode, ownerReadMode, ownerWriteMode, setFileMode, socketMode)
import System.Posix.Signals
import Ares.API
import Ares.App
import Ares.Config
import Ares.FileLock (withFileLockErr, SharedExclusive(Exclusive))
import Ares.IO
import Ares.Manager
import Ares.Process
import Ares.Service
import Ares.WTF

main :: IO ()
main = withConfig $ \c@Config{..} -> do
    hSetBuffering stderr LineBuffering
    createDirectoryIfMissing True dataDir
    setCurrentDirectory dataDir
    setEnv "HOME" dataDir

    m <- newManager . map ($ c) $
            [angelService]
            <> if nginxEnable then [nginxService] else []

    (stop, waitForStop) <- (flip putMVar () &&& readMVar) <$> newEmptyMVar

    mapM_ (\sig -> installHandler sig (Catch stop) Nothing) [sigINT, sigTERM]
    _ <- forkIO (withPrintIOError (waitForManager m) >> stop)
    w <- forkIO (withPrintIOError (runWarp c api (server c m stop)) >> stop)

    reload c m

    waitForStop
    killThread w
    killManager m
    waitForManager m
    exitFailure

server :: Config -> Manager -> IO () -> Server API
server c m stop =
    liftIO (factoryReset c) :<|>
    liftIO (listLogs c) :<|>
    liftIO (reload c m) :<|>
    liftIO stop :<|>
    liftIO (getApps c) :<|>
    (\(AppName name) ->
        liftIO (getApp c name) :<|>
        -- TODO reload after modifications
        liftIO . installApp c name . unAppPath :<|>
        liftIO (getApp c name >>= \case
            Just app -> uninstallApp app >> return True
            _ -> return False))

listLogs :: Config -> IO [FilePath]
listLogs c = do
    aresLogs <- getLogFiles (dataDir c)
    appsLogs <- concatMap logFiles <$> getApps c
    nginxLogs <- getLogFiles (fromMaybe "/var/empty" $ nginxBuiltinLogDir c)
    return $ sortUniq (aresLogs ++ appsLogs ++ nginxLogs)

reload :: Config -> Manager -> IO ()
reload c m = do
    reloadManager m
    reloadWTF c


angelService :: Config -> ServiceConfig
angelService c@Config{..} = ServiceConfig
      { service_name = "angel"
      , service_dataDir = dataDir
      , service_runDir = runDir
      , service_createProcess = proc angelPath [configFile]
      , service_reload = do
          apps <- filter needAngel <$> getApps c
          writeFile configFile (concatMap toAngelEntry apps)
      , service_isNeeded = any needAngel <$> getApps c
      }
  where
    configFile = runDir </> "angel.conf"
    logDir = dataDir </> "log"
    logFile name sub = logDir </> intercalate "." ["angel", name, sub, "log"]
    toAngelEntry App{appName=AppName name,appPath=AppPath path} = unlines
        [ name <> " {"
        , "  exec = " <> show (path </> "run")
        , "  stdout = " <> show (logFile name "stdout")
        , "  stderr = " <> show (logFile name "stderr")
        , "}"
        ]

nginxService :: Config -> ServiceConfig
nginxService c@Config{..} = ServiceConfig
    { service_name = "nginx"
    , service_dataDir = dataDir
    , service_runDir = runDir
    , service_createProcess =
        proc nginxPath ["-c", nginxConfigFile, "-p", prefix]
    , service_reload =
        mapM_ (createDirectoryIfMissing True)
            [ builtinLogDir
            , prefix
            , logDir
            ]
    , service_isNeeded = any needNginx <$> getApps c
    }
  where
    builtinLogDir = fromMaybe (prefix </> "logs") nginxBuiltinLogDir
    logDir = dataDir </> "log"
    prefix = dataDir </> "nginx"


runWarp :: HasServer layout => Config -> Proxy layout -> Server layout -> IO ()
runWarp Config{..} p s = do
    createDirectoryIfMissing True runDir
    withFileLockErr lockFile Exclusive $ \_lock -> do
        withSock sockFile $ \sock -> do
            bind sock (SockAddrUnix sockFile)
            setFileMode sockFile sockMode
            listen sock maxListenQueue
            runSettingsSocket settings sock app
  where
    app = logger (serve p s)
    lockFile = runDir </> "warp.lock"
    logger ap rq respond = do
        rs <- logStdout ap rq respond
        hFlush stdout
        return rs
    settings = setPort port defaultSettings
    sockFile = runDir </> "warp.sock"
    sockMode = socketMode .|. ownerWriteMode .|. ownerReadMode
                          .|. groupWriteMode .|. groupReadMode

withSock :: FilePath -> (Socket -> IO a) -> IO a
withSock sockFile =
    bracket mkSock rmSock
  where
    mkSock = createDirectoryIfMissing True sockDir >> socket AF_UNIX Stream 0
    rmSock sock = close sock >> removeFile sockFile
    sockDir = takeDirectory sockFile
