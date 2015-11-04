{-# LANGUAGE LambdaCase #-}

module Service
    ( ServiceConfig (..)
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
import Process
import FileLock (withFileLockErr, SharedExclusive(Exclusive))

type ServiceResult = Either SomeException ExitCode

data ServiceConfig = ServiceConfig
    { service_name :: String
    , service_dataDir :: FilePath
    , service_runDir :: FilePath
    , service_createProcess :: CreateProcess
    , service_run :: IO () -> IO ()
    , service_isNeeded :: IO Bool
    }

data Service = Service
    { service_config :: ServiceConfig
    , service_processHandle :: ProcessHandle
    , service_resultV :: MVar ServiceResult
    }

startService :: ServiceConfig -> IO Service
startService config = do
    serviceV <- newEmptyMVar
    _ <- forkIO (runService config serviceV `catch` noService config serviceV)
    readMVar serviceV

reloadService :: Service -> IO (Either ExitCode ())
reloadService Service{service_processHandle=ph} =
    withProcessHandle ph $ \case
        OpenHandle pid -> do
            signalProcess sigHUP pid
            return (Right ())
        ClosedHandle e ->
            return (Left e)

stopService :: Service -> IO ()
stopService = terminateProcess . service_processHandle

reapService :: Service -> IO ServiceResult
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

    withFileLockErr lockFile Exclusive $ \_lock -> service_run config $ do
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
