module App
    ( App (..)
    , getApps
    , installApp
    , uninstallApp
    )
  where

import Control.Monad (filterM)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import System.Directory
import System.FilePath
import System.Process

data App = App
    { appName :: String
    , profileDir :: FilePath
    , appPath :: FilePath
    , cliFiles :: [FilePath]
    , needAngel :: Bool
    , needNginx :: Bool
    }
  deriving Show

getApps :: FilePath -> IO [App]
getApps dir =
    catMaybes <$> (mapM (getApp dir) =<< getDirectoryContents dir)

installApp :: FilePath -> String -> FilePath -> IO (Maybe App)
installApp dir name path =
    if isAppName name
        then install
        else return Nothing
  where
    profileBase = dir </> name
    profile = profileBase </> "app"
    install =
        createDirectoryIfMissing True profileBase >>
        spawnProcess "nix-env" ["-p", profile, "--set", path] >>=
        waitForProcess >>
        getApp dir name

uninstallApp :: App -> IO ()
uninstallApp = removeDirectoryRecursive . takeDirectory . profileDir

getApp :: FilePath -> String -> IO (Maybe App)
getApp dir name =
    if isAppName name
        then do
            let profile = dir </> name </> "app"
            path <- canonicalizePath profile
            cli <- getCli path
            needA <- doesNeedAngel path
            needN <- doesNeedNginx path
            return . Just $ App
                { appName = name
                , profileDir = profile
                , appPath = path
                , cliFiles = cli
                , needAngel = needA
                , needNginx = needN
                }
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
