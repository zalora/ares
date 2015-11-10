{-# LANGUAGE RecordWildCards #-}

module WTF
    ( reloadWTF
    )
  where

import System.Directory (renameFile)
import System.FilePath (splitFileName)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import App
import Config

reloadWTF :: Config -> IO ()
reloadWTF c@Config{..} =
    withTempFile dirName fileName $ \tmpFile tmpFH -> do
        hClose tmpFH
        mapM_ (appendFile tmpFile)
            =<< mapM readFile
            =<< map wtfFile <$> getApps c
        renameFile tmpFile wtfdbFile
  where
    (dirName, fileName) = splitFileName wtfdbFile
