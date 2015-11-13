{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ares.CGroup
    ( cgExecCommand
    , cgPrepare
    )
  where

import qualified Data.ByteString.Char8 as BS8
import Control.Monad.Extra (unlessM)
import Data.Maybe (fromMaybe)
import Data.Monoid
import System.Directory (createDirectoryIfMissing)
import System.FilePath
import System.Linux.Mount (mount)
import Ares.Config
import Ares.App


cgExecCommand :: Config -> App -> String
cgExecCommand c@Config{..} App{appName=AppName name,appPath=AppPath path} =
    unwords [ cgexecPath
            , "-g"
            , "name=" <> hierName c <> ":" </> name
            , path </> "run"
            ]

cgPrepare :: Config -> App -> IO ()
cgPrepare c@Config{..} App{appName=AppName name} = do
    unlessM (isMounted mp) $ do
        createDirectoryIfMissing True mp
        mount "cgroup" mp "cgroup" [] $ "none,name=" <> BS8.pack (hierName c)
    createDirectoryIfMissing True (mp </> name)
  where
    mp = runDir </> "cgroup"


-- TODO make sure hierName is sane: "The name should match [\w.-]+"
hierName :: Config -> String
hierName = fromMaybe "ares" . cgHierName

isMounted :: FilePath -> IO Bool
isMounted path =
    any (==path) . map (head . drop 1 . words) . lines
        <$> readFile "/proc/mounts"
