{-# LANGUAGE LambdaCase #-}

module Ares.Service
    ( ServiceConfig (..)
    , ServiceReloadResult
    , ServiceStopResult
    , Service
    , startService
    , reloadService
    , stopService
    , reapService
    )
  where

import Control.Concurrent
import Control.Exception
import Data.Monoid
import System.FilePath
import System.Exit
import System.Directory
import System.Posix.Signals
import Ares.Process
import Ares.FileLock (withFileLockErr, SharedExclusive(Exclusive))


type ServiceReloadResult = Either ExitCode ()
type ServiceStopResult = Either SomeException ExitCode

data ServiceConfig = ServiceConfig
    { service_name :: String
    , service_dataDir :: FilePath
    , service_runDir :: FilePath
    , service_createProcess :: CreateProcess
    , service_reload :: IO ()
    , service_onFailure :: Maybe (IO ())
    }

data Service = Service
    { service_config :: ServiceConfig
    , service_processHandle :: ProcessHandle
    , service_resultV :: MVar ServiceStopResult
    }

startService :: ServiceConfig -> IO Service
startService config = do
    serviceV <- newEmptyMVar
    _ <- forkIO (runService config serviceV `catch` noService config serviceV)
    readMVar serviceV

reloadService :: Service -> IO ServiceReloadResult
reloadService Service{service_config=config,service_processHandle=ph} =
    withProcessHandle ph $ \case
        OpenHandle pid -> do
            service_reload config
            signalProcess sigHUP pid
            return (Right ())
        ClosedHandle e ->
            return (Left e)

stopService :: Service -> IO ()
stopService = terminateProcess . service_processHandle

reapService :: Service -> IO ServiceStopResult
reapService = readMVar . service_resultV


runService :: ServiceConfig -> MVar Service -> IO ()
runService config serviceV = do
    let
        name = service_name config
        dataDir = service_dataDir config
        runDir = service_runDir config
        logDir = dataDir </> "log"
        lockFile = runDir </> (name <> ".lock")
        cp = (service_createProcess config)
                { close_fds = True
                , create_group = True
                }

    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True runDir

    withFileLockErr lockFile Exclusive $ \_lock -> do
        service_reload config
        withCreateProcess cp $ \_ _ _ ph -> do
            resultV <- newEmptyMVar
            putMVar serviceV Service
                { service_config = config
                , service_processHandle = ph
                , service_resultV = resultV
                }
            waitForProcess ph >>= putMVar resultV . Right


noService :: ServiceConfig -> MVar Service -> SomeException -> IO ()
noService config serviceV e = do
    resultV <- newMVar (Left e)
    putMVar serviceV Service
        { service_config = config
        , service_processHandle = throw e
        , service_resultV = resultV
        }
