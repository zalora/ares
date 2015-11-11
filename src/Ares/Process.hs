-- This code was (mostly) copypasted from process-1.3.0.0, System.Process

module Ares.Process
    ( module System.Process
    , module System.Process.Internals
    , withCreateProcess
    )
  where

import Control.Concurrent
import Control.Exception (throwIO)
import Control.Monad
import Foreign.C
import GHC.IO.Exception (IOErrorType(..), IOException(..))
import qualified Control.Exception as C
import System.IO
import System.Process
import System.Process.Internals

withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess c action =
    C.bracketOnError (createProcess c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)


cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr,
                ph@(ProcessHandle _ delegating_ctlc)) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.

    -- However we want to end the Ctl-C handling synchronously, so we'll do
    -- that synchronously, and set delegating_ctlc as False for the
    -- waitForProcess (which would otherwise end the Ctl-C delegation itself).
    when delegating_ctlc
      stopDelegateControlC
    _ <- forkIO (waitForProcess (resetCtlcDelegation ph) >> return ())
    return ()
  where
    resetCtlcDelegation (ProcessHandle m _) = ProcessHandle m False

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e
