{-# LANGUAGE LambdaCase #-}

module Angel
    ( AngelConfig (..)
    , Angel
    , startAngel
    , reloadAngel
    , stopAngel
    , reapAngel
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
import App

type AngelResult = Either SomeException ExitCode

data AngelConfig = AngelConfig
    { angel_dataDir :: FilePath
    , angel_runDir :: FilePath
    , angel_profilesDir :: FilePath
    }

data Angel = Angel
    { angel_config :: AngelConfig
    , angel_processHandle :: ProcessHandle
    , angel_resultV :: MVar AngelResult
    }

startAngel :: AngelConfig -> IO Angel
startAngel config = do
    angelV <- newEmptyMVar
    _ <- forkIO (runAngel config angelV `catch` noAngel config angelV)
    readMVar angelV

reloadAngel :: Angel -> IO (Either ExitCode ())
reloadAngel Angel{angel_processHandle=ph} =
    withProcessHandle ph $ \case
        OpenHandle pid -> do
            signalProcess sigHUP pid
            return (Right ())
        ClosedHandle e ->
            return (Left e)

stopAngel :: Angel -> IO ()
stopAngel = terminateProcess . angel_processHandle

reapAngel :: Angel -> IO AngelResult
reapAngel = readMVar . angel_resultV


runAngel :: AngelConfig -> MVar Angel -> IO ()
runAngel config angelV = do
    let
        dataDir = angel_dataDir config
        profilesDir = angel_profilesDir config
        runDir = angel_runDir config
        logDir = dataDir </> "log"
        lockFile = runDir </> "angel.lock"
        configFile = runDir </> "angel.conf"
        cp = (proc "angel" [configFile])
                { close_fds = True
                , create_group = True
                }

    createDirectoryIfMissing True logDir
    createDirectoryIfMissing True runDir

    withFileLockErr lockFile Exclusive $ \_lock -> do
        writeFile configFile =<< toAngelConfig logDir <$> getApps profilesDir
        withCreateProcess cp $ \_ _ _ ph -> do
            resultV <- newEmptyMVar
            putMVar angelV Angel
                { angel_config = config
                , angel_processHandle = ph
                , angel_resultV = resultV
                }
            waitForProcess ph >>= putMVar resultV . Right


noAngel :: AngelConfig -> MVar Angel -> SomeException -> IO ()
noAngel config angelV e = do
    resultV <- newMVar (Left e)
    putMVar angelV Angel
        { angel_config = config
        , angel_processHandle = throw e
        , angel_resultV = resultV
        }

toAngelConfig :: FilePath -> [App] -> String
toAngelConfig logDir =
    concatMap f . filter needAngel
  where
    f app@App{appName=name,appPath=path} =
        name <> " {\n" <>
        "  exec = " <> show (path </> "run") <> "\n" <>
        "  stdout = " <> show (logFile app "stdout") <> "\n" <>
        "  stderr = " <> show (logFile app "stderr") <> "\n" <>
        "}\n"
    logFile App{appName=name} logName =
        logDir </> ("angel." <> name <> "." <> logName <> ".log")
