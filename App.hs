module App
    ( App (..)
    , getApps
    )
  where

import Control.Monad (filterM)
import Data.List (zipWith5)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as Text
import System.Directory
import System.FilePath

data App = App
    { appName :: String
    , appPath :: FilePath
    , cliFiles :: [FilePath]
    , needAngel :: Bool
    , needNginx :: Bool
    }
  deriving Show

getApps :: FilePath -> IO [App]
getApps profilesDir = do
    names <- filter isAppName <$> getDirectoryContents profilesDir
    paths <- mapM toAppPath names
    clis <- mapM getCli paths
    angelNeeds <- mapM doesNeedAngel paths
    nginxNeeds <- mapM doesNeedNginx paths
    return (zipWith5 App names paths clis angelNeeds nginxNeeds)
  where
    isAppName = either (const False) (const True) . parseAppName

    toAppPath = canonicalizePath . (profilesDir </>) . (</> "app")

    getCli path = do
        let cliDir = path </> "bin"
        isDir <- doesDirectoryExist cliDir
        if isDir
            then filterM doesFileExist
                            =<< mapM (canonicalizePath . (cliDir </>))
                            =<< getDirectoryContents cliDir
            else return []

    doesNeedAngel = doesFileExist . (</> "run")

    doesNeedNginx path =
        any (==True) <$> mapM (doesFileExist . ((path </> "nginx") </>))
            [ "default-locations.conf"
            , "servers.conf"
            ]

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
