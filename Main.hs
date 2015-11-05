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

    m <- newManager $ map ($ c)
            [ angelService
            , nginxService
            ]

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
    "reload" :>
        Post '[JSON] Bool :<|>
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
server Config{..} m stop =
    liftIO (reloadManager m) :<|>
    liftIO stop :<|>
    liftIO (getApps profilesDir) :<|>
    (\(AppName name) ->
        liftIO (getApp profilesDir name) :<|>
        -- TODO reload after modifications
        liftIO . installApp profilesDir name . unAppPath :<|>
        liftIO (getApp profilesDir name >>= \case
            Just app -> uninstallApp app >> return True
            _ -> return False))


angelService :: Config -> ServiceConfig
angelService Config{..} = ServiceConfig
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
nginxService Config{..} = ServiceConfig
    { service_name = "nginx"
    , service_dataDir = dataDir
    , service_runDir = runDir
    , service_createProcess = proc "nginx" ["-c", nginxConfigFile, "-p", prefix]
    , service_run = \continue -> do
        mapM_ (createDirectoryIfMissing True)
            [ builtinLogDir
            , prefix
            , logDir
            ]
        continue
    , service_isNeeded = any needNginx <$> getApps profilesDir
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
        runSettingsSocket settings sock (serve p s)
  where
    settings = setPort port defaultSettings
    sockFile = runDir </> "warp.sock"

withSock :: FilePath -> (Socket -> IO a) -> IO a
withSock sockFile =
    bracket mkSock rmSock
  where
    mkSock = createDirectoryIfMissing True sockDir >> socket AF_UNIX Stream 0
    rmSock sock = close sock >> removeFile sockFile
    sockDir = takeDirectory sockFile
