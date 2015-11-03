module Main (main) where

import Control.Arrow
import Control.Concurrent
import Data.Monoid
import System.Directory
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Posix.Signals
import App
import Service
import Process


main :: IO ()
main = do
    hSetBuffering stderr LineBuffering

    angel <- startService angelService
    nginx <- startService nginxService

    (stop, waitForStop) <- (flip putMVar () &&& readMVar) <$> newEmptyMVar

    _ <- installHandler keyboardSignal (Catch stop) Nothing
    _ <- forkIO (reapService angel >> stop)
    _ <- forkIO (reapService nginx >> stop)

    waitForStop
    hPutStrLn stderr "Stopping..."
    stopService angel
    stopService nginx

    hPutStrLn stderr . ("Angel result: " <>) . show =<< reapService angel
    hPutStrLn stderr . ("Nginx result: " <>) . show =<< reapService nginx

    exitFailure


angelService :: ServiceConfig
angelService = service where
    dataDir = "/tmp/zalora/data/angel"
    runDir = "/tmp/zalora/run/angel"
    logDir = dataDir </> "log"
    configFile = runDir </> "angel.conf"
    profilesDir = "/nix/var/nix/profiles/per-user/zalora/ares-apps"
    service = ServiceConfig
        { service_name = "angel"
        , service_dataDir = dataDir
        , service_runDir = runDir
        , service_createProcess = proc "angel" [configFile]
        , service_run = \continue -> do
            -- TODO exceptions?
            writeFile configFile =<< toAngelConfig <$> getApps profilesDir
            continue
        }
    toAngelConfig = concatMap toAngelEntry . filter needAngel
    toAngelEntry App{appName=name,appPath=path} =
        name <> " {\n" <>
        "  exec = " <> show (path </> "run") <> "\n" <>
        "  stdout = " <> show (logFile name "stdout") <> "\n" <>
        "  stderr = " <> show (logFile name "stderr") <> "\n" <>
        "}\n"
    logFile name logName =
        logDir </> ("angel." <> name <> "." <> logName <> ".log")

nginxService :: ServiceConfig
nginxService = service where
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
        }
