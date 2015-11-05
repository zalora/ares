{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Config
    ( Config (..)
    , Port
    , withConfig
    )
  where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (eitherDecode, FromJSON)
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Port)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

data Config = Config
    { port :: Port
    , profilesDir :: FilePath
    , sockFile :: FilePath
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
