{-# LANGUAGE LambdaCase #-}

module Nginx
    ( NginxConfig (..)
    , Nginx
    , startNginx
    , reloadNginx
    , stopNginx
    , reapNginx
    )
  where

import Control.Concurrent
import Control.Exception
import System.FilePath
import System.Exit
import System.Directory
import System.Posix.Signals
import Process
import FileLock (withFileLockErr, SharedExclusive(Exclusive))

type NginxResult = Either SomeException ExitCode

data NginxConfig = NginxConfig
    { nginx_dataDir :: FilePath
    , nginx_runDir :: FilePath
    , nginx_configFile :: FilePath
    }

data Nginx = Nginx
    { nginx_config :: NginxConfig
    , nginx_processHandle :: ProcessHandle
    , nginx_resultV :: MVar NginxResult
    }

startNginx :: NginxConfig -> IO Nginx
startNginx config = do
    nginxV <- newEmptyMVar
    _ <- forkIO (runNginx config nginxV `catch` noNginx config nginxV)
    readMVar nginxV

reloadNginx :: Nginx -> IO (Either ExitCode ())
reloadNginx Nginx{nginx_processHandle=ph} =
    withProcessHandle ph $ \case
        OpenHandle pid -> do
            signalProcess sigHUP pid
            return (Right ())
        ClosedHandle e ->
            return (Left e)

stopNginx :: Nginx -> IO ()
stopNginx = terminateProcess . nginx_processHandle

reapNginx :: Nginx -> IO NginxResult
reapNginx = readMVar . nginx_resultV


runNginx :: NginxConfig -> MVar Nginx -> IO ()
runNginx config nginxV = do
    let
        dataDir = nginx_dataDir config
        runDir = nginx_runDir config
        configFile = nginx_configFile config
        logDir = dataDir </> "log"
        defLogDir = dataDir </> "logs"
        lockFile = runDir </> "nginx.lock"
        cp = (proc "nginx"
                [ "-c", configFile
                , "-g", "error_log stderr warn;"
                , "-p", dataDir
                ])
                { close_fds = True
                , create_group = True
                }

    createDirectoryIfMissing True dataDir
    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True runDir

    -- Silence warnings on startup by ensuring the default log directory exists.
    createDirectoryIfMissing True defLogDir

    withFileLockErr lockFile Exclusive $ \_lock -> do
        withCreateProcess cp $ \_ _ _ ph -> do
            resultV <- newEmptyMVar
            putMVar nginxV Nginx
                { nginx_config = config
                , nginx_processHandle = ph
                , nginx_resultV = resultV
                }
            waitForProcess ph >>= putMVar resultV . Right


noNginx :: NginxConfig -> MVar Nginx -> SomeException -> IO ()
noNginx config nginxV e = do
    resultV <- newMVar (Left e)
    putMVar nginxV Nginx
        { nginx_config = config
        , nginx_processHandle = throw e
        , nginx_resultV = resultV
        }
