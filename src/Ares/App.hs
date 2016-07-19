{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ares.App
    ( App (..)
    , AppName (AppName, unAppName)
    , AppPath (AppPath, unAppPath)
    , factoryReset
    , getApp
    , getApps
    , installApp
    , uninstallApp

    , getLogFiles
    )
  where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Control.Monad (filterM)
import Control.Monad.Extra (ifM, unlessM, whenM)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Servant (FromFormUrlEncoded (fromFormUrlEncoded), FromText (fromText))
import System.Directory
import System.FilePath
import System.Process
import Ares.Config

data App = App
    { appName :: AppName
    , appPath :: AppPath
    , appDataDir :: FilePath
    , cliFiles :: Map String FilePath
    , logFiles :: [FilePath]
    , diagsFile :: FilePath
    , profileDir :: FilePath
    }
  deriving
    ( Generic
    , Show
    )

instance ToJSON App

newtype AppName = AppName { unAppName :: String }

instance FromText AppName where
    fromText =
        either (const Nothing) (Just . AppName) . parseAppName . Text.unpack

instance Show AppName
    where show = unAppName

instance ToJSON AppName
    where toJSON = toJSON . unAppName

newtype AppPath = AppPath { unAppPath :: FilePath }

instance FromFormUrlEncoded AppPath where
    fromFormUrlEncoded = \case
        [(x,"")] -> parseAppPath x
        _ -> Left "no Nix store path"

instance Show AppPath where
    show = unAppPath

instance ToJSON AppPath where
    toJSON = toJSON . unAppPath

factoryReset :: Config -> IO ()
factoryReset c@Config{..} = do
    whenM (doesDirectoryExist profilesDir)
          (removeDirectoryRecursive profilesDir)
    createDirectoryIfMissing True profilesDir
    mapM_ (uncurry (installApp c)) (Map.toList builtinApps)

getApps :: Config -> IO [App]
getApps c@Config{..} = do
    unlessM (doesDirectoryExist profilesDir)
            (factoryReset c)
    catMaybes <$> (mapM (getApp c) =<< getDirectoryContents profilesDir)

installApp :: Config -> String -> FilePath -> IO (Maybe App)
installApp c@Config{..} name path =
    if isAppName name
        then install
        else return Nothing
  where
    install = do
        let dirName = profilesDir </> name
            profile = dirName </> "app"
        createDirectoryIfMissing True dirName
        ph <- spawnProcess nixEnvPath ["-p", profile, "--set", path]
        _ <- waitForProcess ph
        getApp c name

uninstallApp :: App -> IO ()
uninstallApp = removeDirectoryRecursive . profileDir

getApp :: Config -> String -> IO (Maybe App)
getApp Config{..} name =
    if isAppName name
        then do
            let dirName = profilesDir </> name
                profile = dirName </> "app"
                aDataDir = appsDataDirRoot </> name
            exists <- doesDirectoryExist profile
            if exists
                then do
                    path <- canonicalizePath profile
                    cli <- getBinFiles path
                    logs <- getLogFiles aDataDir
                    diags <- getDiagsFile path
                    return . Just $ App
                        { appName = AppName name
                        , appPath = AppPath path
                        , appDataDir = aDataDir
                        , cliFiles = cli
                        , logFiles = logs
                        , diagsFile = diags
                        , profileDir = dirName
                        }
                else return Nothing
        else return Nothing

getDiagsFile :: FilePath -> IO FilePath
getDiagsFile path = do
    let diagsFile = path </> "diags.json"
    ifM (doesFileExist diagsFile)
        (return diagsFile)
        (return "/dev/null")

getBinFiles :: FilePath -> IO (Map String FilePath)
getBinFiles path = do
    let binDir = path </> "bin"
    ifM (doesDirectoryExist binDir)
        (do names <- getDirectoryContents binDir
            paths <- mapM (canonicalizePath . (binDir </>)) names
            Map.fromList <$> filterM (doesFileExist . snd) (zip names paths))
        (return Map.empty)

getLogFiles :: FilePath -> IO [FilePath]
getLogFiles path = do
    let logDir = path </> "log"
    ifM (doesDirectoryExist logDir)
        (filterM doesFileExist
            =<< mapM (canonicalizePath . (logDir </>))
            =<< getDirectoryContents logDir)
        (return [])

isAppName :: FilePath -> Bool
isAppName = either (const False) (const True) . parseAppName

-- `storeName` is based on [`checkStoreName`][1].
--
-- [1]: https://github.com/NixOS/nix/blob/master/src/libstore/store-api.cc
storeName :: Atto.Parser Text
storeName = Text.cons <$> Atto.satisfy headChar <*> Atto.takeWhile tailChar
  where headChar = Atto.inClass "A-Za-z0-9+_?=-" -- no '.' in head
        tailChar = Atto.inClass "A-Za-z0-9+._?=-"

parseAppName :: FilePath -> Either String String
parseAppName =
    fmap Text.unpack . Atto.parseOnly (storeName <* Atto.endOfInput) . Text.pack

parseAppPath :: Text -> Either String AppPath
parseAppPath =
    fmap (AppPath . Text.unpack) . Atto.parseOnly (storePath1 <* Atto.endOfInput)
  where
    storePath1 :: Atto.Parser Text
    storePath1 = (<>) <$> Atto.string "/nix/store/" <*> storeName
