{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Arrow ((&&&))
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Monoid
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Posix.Signals
import App
import Service
import Manager
import Process
import Servant
import Network.Socket
import Network.Wai (Application)
import Network.Wai.Handler.Warp
    (defaultSettings, runSettingsSocket, setPort, Port)


data Config = Config
    { port :: Port
    , profilesDir :: FilePath
    , sockFile :: FilePath
    }

main :: IO ()
main = do
    hSetBuffering stderr LineBuffering

    let c@Config{..} = Config
            { port = 3000
            , profilesDir = "/nix/var/nix/profiles/per-user/zalora/ares-apps"
            , sockFile = "/tmp/zalora/ares.sock"
            }

    m <- newManager $ map ($ c)
            [ angelService
            , nginxService
            ]

    (stop, waitForStop) <- (flip putMVar () &&& readMVar) <$> newEmptyMVar

    _ <- installHandler keyboardSignal (Catch stop) Nothing
    _ <- forkIO (waitForManager m >> stop)

    w <- forkIO (runLocal port sockFile (serve api (server c m)))

    waitForStop
    killThread w
    killManager m
    waitForManager m
    exitFailure


api :: Proxy API
api = Proxy

type API =
    "reload" :>
        Post '[JSON] Bool :<|>
    "apps" :> (
        Get '[JSON] [App] :<|>
        Capture "name" AppName :> (
            Get '[JSON] (Maybe App) :<|>
            ReqBody '[FormUrlEncoded] AppPath :>
                Put '[JSON] (Maybe App) :<|>
            Delete '[JSON] Bool ))

server :: Config -> Manager -> Server API
server Config{..} m =
    liftIO (reloadManager m) :<|>
    liftIO (getApps profilesDir) :<|>
    (\(AppName name) ->
        liftIO (getApp profilesDir name) :<|>
        -- TODO reload after modifications
        liftIO . installApp profilesDir name . unAppPath :<|>
        liftIO (getApp profilesDir name >>= \case
            Just app -> uninstallApp app >> return True
            _ -> return False))


angelService :: Config -> ServiceConfig
angelService Config{..} = service where
    dataDir = "/tmp/zalora/data/angel"
    runDir = "/tmp/zalora/run/angel"
    logDir = dataDir </> "log"
    configFile = runDir </> "angel.conf"
    service = ServiceConfig
        { service_name = "angel"
        , service_dataDir = dataDir
        , service_runDir = runDir
        , service_createProcess = proc "angel" [configFile]
        , service_run = \continue -> do
            -- TODO exceptions?
            writeFile configFile =<< toAngelConfig <$> getApps profilesDir
            continue
        , service_isNeeded = any needAngel <$> getApps profilesDir
        }
    toAngelConfig = concatMap toAngelEntry . filter needAngel
    toAngelEntry App{appName=AppName name,appPath=AppPath path} =
        name <> " {\n" <>
        "  exec = " <> show (path </> "run") <> "\n" <>
        "  stdout = " <> show (logFile name "stdout") <> "\n" <>
        "  stderr = " <> show (logFile name "stderr") <> "\n" <>
        "}\n"
    logFile name logName =
        logDir </> ("angel." <> name <> "." <> logName <> ".log")

nginxService :: Config -> ServiceConfig
nginxService Config{..} = service where
    dataDir = "/tmp/zalora/data/nginx"
    runDir = "/tmp/zalora/run/nginx"
    defLogDir = dataDir </> "logs"
    configFile = dataDir </> "nginx.conf"
    service = ServiceConfig
        { service_name = "nginx"
        , service_dataDir = dataDir
        , service_runDir = runDir
        , service_createProcess = proc "nginx" ["-c", configFile, "-p", dataDir]
        , service_run = \continue -> do
            -- Silence warnings on startup by ensuring the default log
            -- directory exists.
            createDirectoryIfMissing True defLogDir
            continue
        , service_isNeeded = any needNginx <$> getApps profilesDir
        }


runLocal :: Port -> FilePath -> Application -> IO ()
runLocal port sockFile app =
    bracket mkSock rmSock $ \sock -> do
        bind sock (SockAddrUnix sockFile)
        listen sock maxListenQueue
        runSettingsSocket settings sock app
  where
    mkSock = socket AF_UNIX Stream 0
    rmSock sock = close sock >> removeFile sockFile
    settings = setPort port defaultSettings
