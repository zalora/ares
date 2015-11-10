{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module SyslogNG
    ( reloadSyslogNG
    )
  where

import Data.List (intercalate, isSuffixOf)
import Data.Monoid
import System.Directory (renameFile)
import System.FilePath (splitFileName, takeBaseName)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import App
import Config

-- TODO syslog-ng service
reloadSyslogNG :: Config -> IO ()
reloadSyslogNG c@Config{..} =
    withTempFile dirName fileName $ \tmpFile tmpFH -> do
        hClose tmpFH
        mapM_ (appendFile tmpFile)
            =<< map formatSyslogNGConfig <$> getApps c
        renameFile tmpFile syslogNGFile
  where
    (dirName, fileName) = splitFileName syslogNGFile

formatSyslogNGConfig :: App -> String
formatSyslogNGConfig app@App{..} =
    concatMap (formatSyslogNGEntry app) logFiles

formatSyslogNGEntry :: App -> FilePath -> String
formatSyslogNGEntry App{..} logFilePath = unlines . filter (not . null) $
    [ "source " <> sourceName <> " {"
    , "  file(" <> show logFilePath <> " " <> sourceOptions <> ");"
    , "};"
    , "rewrite " <> rewriteName <> " {"
    , "  set(" <> show appName <> ", value(\"PROGRAM\"));"
    , "  set(" <> show logFilePath <> ", value(\"log_file_path\"));"
    , "};"
    , "log {"
    , "  source(" <> sourceName <> ");"
    , maybe "" (("  parser("<>) . (<>");") . show) parserName
    , "  rewrite(" <> rewriteName <> ");"
    , "  destination(d_target);"
    , "};"
    ]
  where
    sourceOptions = logFileSourceOptions logType
    parserName = logParserName logType
    logType = toLogType logFilePath
    logName = unAppName appName <> "-" <> takeBaseName logFilePath
    sourceName = "s_" <> logName
    rewriteName = "r_" <> logName

data LogType
    = MultiLineFormat
    | NginxAccessFormat
    | SimpleFormat

toLogType :: FilePath -> LogType
toLogType path = if
    | isSuffixOf "nginx-error.log" path -> MultiLineFormat
    | isSuffixOf "nginx-access.log" path -> NginxAccessFormat
    | otherwise -> SimpleFormat

logFileSourceOptions :: LogType -> String
logFileSourceOptions = \case
    MultiLineFormat -> unwords
        [ "multi-line-mode(regexp)"
        , "multi-line-prefix(" <> show regexp <> ")"
        ]
      where
        regexp = intercalate "|"
            [ "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
            , "[0-9]{6} [0-9]{2}:[0-9]{2}:[0-9]{2}"
            , "[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}"
            ]
    NginxAccessFormat -> unwords
        [ "flags(no-parse)"
        , "follow-freq(1)"
        , "multi-line-mode(indented)"
        ]
    SimpleFormat -> unwords
        [ "flags(no-parse)"
        , "follow-freq(1)"
        ]

logParserName :: LogType -> Maybe String
logParserName = \case
    NginxAccessFormat -> Just "p_httpd"
    _ -> Nothing
