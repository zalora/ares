module Ares.IO
    ( withPrintIOError
    )
  where

import System.IO (hPutStrLn, stderr)
import System.IO.Error (catchIOError)

printIOError :: IOError -> IO ()
printIOError = hPutStrLn stderr . show

withPrintIOError :: IO () -> IO ()
withPrintIOError a = a `catchIOError` printIOError
