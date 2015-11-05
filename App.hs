{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module App
    ( App (..)
    , AppName (AppName, unAppName)
    , AppPath (AppPath, unAppPath)
    , getApp
    , getApps
    , installApp
    , uninstallApp
    )
  where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import Control.Monad (filterM)
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.Text (Text)
import GHC.Generics
import Servant (FromFormUrlEncoded (fromFormUrlEncoded), FromText (fromText))
import System.Directory
import System.FilePath
import System.Process
import Config

data App = App
    { appName :: AppName
    , appPath :: AppPath
    , cliFiles :: [FilePath]
    , needAngel :: Bool
    , needNginx :: Bool
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


getApps :: Config -> IO [App]
getApps c@Config{..} =
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
            exists <- doesDirectoryExist profile
            if exists
                then do
                    path <- canonicalizePath profile
                    cli <- getCli path
                    needA <- doesNeedAngel path
                    needN <- doesNeedNginx path
                    return . Just $ App
                        { appName = AppName name
                        , appPath = AppPath path
                        , cliFiles = cli
                        , needAngel = needA
                        , needNginx = needN
                        , profileDir = dirName
                        }
                else return Nothing
        else return Nothing

getCli :: FilePath -> IO [FilePath]
getCli path = do
    let cliDir = path </> "bin"
    isDir <- doesDirectoryExist cliDir
    if isDir
        then filterM doesFileExist
                        =<< mapM (canonicalizePath . (cliDir </>))
                        =<< getDirectoryContents cliDir
        else return []

doesNeedAngel :: FilePath -> IO Bool
doesNeedAngel = doesFileExist . (</> "run")

doesNeedNginx :: FilePath -> IO Bool
doesNeedNginx path =
    any (==True) <$> mapM (doesFileExist . ((path </> "nginx") </>))
        [ "default-locations.conf"
        , "servers.conf"
        ]

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
