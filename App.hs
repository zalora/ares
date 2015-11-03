module App
    ( App (..)
    , getApps
    )
  where

import Control.Monad (filterM)
import Data.Maybe (catMaybes)
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
getApps dir =
    catMaybes <$> (mapM (getApp dir) =<< getDirectoryContents dir)

getApp :: FilePath -> String -> IO (Maybe App)
getApp dir name =
    if isAppName name
        then do
            path <- toAppPath name
            cli <- getCli path
            needA <- doesNeedAngel path
            needN <- doesNeedNginx path
            return . Just $ App name path cli needA needN
        else return Nothing
  where
    toAppPath = canonicalizePath . (dir </>) . (</> "app")

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
