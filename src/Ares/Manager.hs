module Ares.Manager
    ( Manager
    , newManager
    , reloadManager
    , killManager
    , waitForManager
    )
  where

import Control.Concurrent
import Control.Conditional ((<&&>), ifM)
import Control.Monad (filterM, when)
import Control.Monad.Extra (partitionM, whenM)
import Data.Maybe (catMaybes, fromJust, isJust)
import Ares.Service


data Manager = Manager
    { manager_services :: [ManagedService]
    , manager_deadV :: MVar ()
    }

newManager :: [ServiceConfig] -> IO Manager
newManager configs =
    Manager <$> mapM (\s -> ManagedService s <$> newEmptyMVar) configs
            <*> newEmptyMVar

reloadManager :: Manager -> IO ()
reloadManager m = do
    (stopped, started) <- partitionM ms_isStarted (manager_services m)
    (needReload, needStop) <- partitionM ms_isNeeded started
    needStart <- filterM ms_isNeeded stopped
    mapM_ (ms_start m) needStart
    mapM_ (ms_stop m) needStop
    mapM_ (ms_reload m) needReload

killManager :: Manager -> IO ()
killManager m = do
    _ <- tryPutMVar (manager_deadV m) ()
    mapM_ stopService =<< runningServices m

waitForManager :: Manager -> IO ()
waitForManager m@Manager{manager_deadV=deadV} = do
    () <- readMVar deadV
    services <- runningServices m
    mapM_ stopService services
    mapM_ reapService services

isManagerAlive :: Manager -> IO Bool
isManagerAlive Manager{manager_deadV=deadV} =
    maybe True (const False) <$> tryReadMVar deadV

runningServices :: Manager -> IO [Service]
runningServices = fmap catMaybes . mapM ms_maybeService . manager_services


data ManagedService = ManagedService
    { ms_config :: ServiceConfig
    , ms_serviceV :: MVar Service
    }

ms_isNeeded :: ManagedService -> IO Bool
ms_isNeeded = service_isNeeded . ms_config

ms_isStarted :: ManagedService -> IO Bool
ms_isStarted = isEmptyMVar . ms_serviceV

ms_maybeService :: ManagedService -> IO (Maybe Service)
ms_maybeService = tryReadMVar . ms_serviceV

ms_reload :: Manager -> ManagedService -> IO ServiceReloadResult
ms_reload _ ms = reloadService =<< readMVar (ms_serviceV ms)

ms_onFailure :: ManagedService -> Maybe (IO ())
ms_onFailure = service_onFailure . ms_config

ms_start :: Manager -> ManagedService -> IO Bool
ms_start m ms = do
    s <- startService (ms_config ms)
    r <- tryPutMVar (ms_serviceV ms) s
    when r $ do
        _ <- forkIO $ do
            _ <- reapService s
            _ <- tryTakeMVar (ms_serviceV ms)
            whenM (ms_isNeeded ms)
                  (ifM (pure (isJust (ms_onFailure ms)) <&&> isManagerAlive m)
                       (fromJust (ms_onFailure ms) >> reloadManager m)
                       (killManager m))
        return ()
    return r

ms_stop :: Manager -> ManagedService -> IO ServiceStopResult
ms_stop _ ms = do
    s <- takeMVar (ms_serviceV ms)
    stopService s
    reapService s
