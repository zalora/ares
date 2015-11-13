{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Ares.Config
    ( Config (..)
    , Port
    , withConfig
    )
  where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (eitherDecode, FromJSON)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

data Config = Config
    { port :: Port
    , profilesDir :: FilePath
    , appsDataDirRoot :: FilePath
    , dataDir :: FilePath
    , runDir :: FilePath
    , builtinApps :: Map String FilePath
    , wtfdbFile :: FilePath
    , cgHierName :: Maybe String
        -- ^ Name to be used when mounting a cgroup hierarchy.
        -- If set to `Nothing`, then use `"ares"`.
    , nginxEnable :: Bool
    , nginxConfigFile :: FilePath
    , nginxBuiltinLogDir :: Maybe FilePath
        -- ^ Directory for logs configured at build time[1].
        -- If set to `Nothing`, then use `dataDir </> "logs"`.
        --
        -- [1]: http://nginx.org/en/docs/configure.html
    , angelPath :: FilePath
    , cgexecPath :: FilePath
    , nginxPath :: FilePath
    , nixEnvPath :: FilePath
    }
  deriving (Generic)

instance FromJSON Config

withConfig :: (Config -> IO ()) -> IO ()
withConfig f = getArgs >>= \case
    args | "--help" `elem` args ->
        hPutStrLn stderr usage
    [configFile] -> eitherDecode <$> LBS.readFile configFile >>= \case
        Right c -> f c
        Left e -> error (configFile <> ": eitherDecode: " <> e)
    args ->
        error (unlines ["bad args: " <> show args] <> usage)
  where
    usage = "Usage: ares [--help] CONFIG_FILE"
