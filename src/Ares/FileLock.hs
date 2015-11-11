{-# LANGUAGE LambdaCase #-}

module Ares.FileLock
    ( module System.FileLock
    , withFileLockErr
    )
  where

import Control.Exception (bracket)
import System.FileLock
import System.IO.Error (mkIOError, alreadyInUseErrorType)


withFileLockErr :: FilePath -> SharedExclusive -> (FileLock -> IO a) -> IO a
withFileLockErr path mode f =
    bracket (tryLockFile path mode) (whenJust unlockFile) $ \case
        Just lock -> f lock
        Nothing -> ioError alreadyLocked
  where
    alreadyLocked = mkIOError alreadyInUseErrorType name Nothing (Just path)
    name = "FileLock.withFileLockErr"


-- TODO find a better place for whenJust
whenJust :: (a -> IO ()) -> Maybe a -> IO ()
whenJust = maybe (return ())
