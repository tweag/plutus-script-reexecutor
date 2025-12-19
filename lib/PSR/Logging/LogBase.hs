module PSR.Logging.LogBase (withStdOutLogging) where

import PSR.Logging.Interface (LogSeverity (..), Logger (..))

import Data.Aeson (ToJSON (..), Value (Null))
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Log qualified as L
import Log.Backend.StandardOutput (withJsonStdOutLogger)

withStdOutLogging :: Text -> ((Logger, L.LoggerEnv) -> IO ()) -> IO ()
withStdOutLogging appName cont = do
    withJsonStdOutLogger $ \logger -> do
        let logEnv' = mkLoggerEnv appName logger
            logInterface =
                Logger
                    { logAtLevel = logBaseIO
                    , logAtLevelWith = logBaseIOJSON
                    , logEnv = logEnv'
                    }
        cont (logInterface, logEnv')

logBaseIO :: L.LoggerEnv -> LogSeverity -> Text -> IO ()
logBaseIO env sev msg = logBaseIOJSON env sev msg Null

logBaseIOJSON :: (ToJSON a) => L.LoggerEnv -> LogSeverity -> Text -> a -> IO ()
logBaseIOJSON env sev msg val = do
    now <- getCurrentTime
    L.logMessageIO env now (convertSeverity sev) msg (toJSON val)

mkLoggerEnv :: Text -> L.Logger -> L.LoggerEnv
mkLoggerEnv appName logger =
    L.LoggerEnv
        { leLogger = logger
        , leComponent = appName
        , leDomain = []
        , leData = []
        , leMaxLogLevel = L.LogTrace
        }

convertSeverity :: LogSeverity -> L.LogLevel
convertSeverity = \case
    Debug -> L.LogTrace
    Message -> L.LogInfo
    Warning -> L.LogAttention
    Error -> L.LogAttention
