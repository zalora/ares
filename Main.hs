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
import Data.List (intercalate)
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
import System.Posix.Signals
import App
import Config
import Manager
import Process
import Service

main :: IO ()
main = withConfig $ \c@Config{..} -> do
    hSetBuffering stderr LineBuffering
    setCurrentDirectory dataDir
    setEnv "HOME" dataDir

    m <- newManager . map ($ c) $
            [angelService]
            <> if nginxEnable then [nginxService] else []

    (stop, waitForStop) <- (flip putMVar () &&& readMVar) <$> newEmptyMVar

    mapM_ (\sig -> installHandler sig (Catch stop) Nothing) [sigINT, sigTERM]
    _ <- forkIO (waitForManager m >> stop)
    w <- forkIO (runWarp c api (server c m stop) >> stop)

    waitForStop
    killThread w
    killManager m
    waitForManager m
    exitFailure

api :: Proxy API
api = Proxy

type API =
    "factory-reset" :>
        Post '[] () :<|>
    "reload" :>
        Post '[] () :<|>
    "stop" :>
        Post '[] () :<|>
    "apps" :> (
        Get '[JSON] [App] :<|>
        Capture "name" AppName :> (
            Get '[JSON] (Maybe App) :<|>
            ReqBody '[FormUrlEncoded] AppPath :>
                Put '[JSON] (Maybe App) :<|>
            Delete '[JSON] Bool ))

server :: Config -> Manager -> IO () -> Server API
server c m stop =
    liftIO (factoryReset c) :<|>
    liftIO (reloadManager m) :<|>
    liftIO stop :<|>
    liftIO (getApps c) :<|>
    (\(AppName name) ->
        liftIO (getApp c name) :<|>
        -- TODO reload after modifications
        liftIO . installApp c name . unAppPath :<|>
        liftIO (getApp c name >>= \case
            Just app -> uninstallApp app >> return True
            _ -> return False))


angelService :: Config -> ServiceConfig
angelService c@Config{..} = ServiceConfig
      { service_name = "angel"
      , service_dataDir = dataDir
      , service_runDir = runDir
      , service_createProcess = proc angelPath [configFile]
      , service_reload =
          writeFile configFile =<< toAngelConfig <$> getApps c
      , service_isNeeded = any needAngel <$> getApps c
      }
  where
    configFile = runDir </> "angel.conf"
    logDir = dataDir </> "log"
    logFile name sub = logDir </> intercalate "." ["angel", name, sub, "log"]
    toAngelConfig = concatMap toAngelEntry . filter needAngel
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
runWarp Config{..} p s =
    withSock sockFile $ \sock -> do
        bind sock (SockAddrUnix sockFile)
        listen sock maxListenQueue
        runSettingsSocket settings sock app
  where
    app = logger (serve p s)
    logger ap rq respond = do
        rs <- logStdout ap rq respond
        hFlush stdout
        return rs
    settings = setPort port defaultSettings
    sockFile = runDir </> "warp.sock"

withSock :: FilePath -> (Socket -> IO a) -> IO a
withSock sockFile =
    bracket mkSock rmSock
  where
    mkSock = createDirectoryIfMissing True sockDir >> socket AF_UNIX Stream 0
    rmSock sock = close sock >> removeFile sockFile
    sockDir = takeDirectory sockFile
