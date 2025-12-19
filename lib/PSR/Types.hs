{-# LANGUAGE TemplateHaskell #-}

module PSR.Types (
    App (..),
    runApp,
    module Export,
    AppConfig (..),
    HasConfigMap (..),
    HasLoggerEnv (..),
    ChainSyncEvent (..),
    ChainSyncEventException (..),
    Block (..),
    isByron,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Exception (Exception)
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import GHC.Generics (Generic)
import Lens.Micro.Platform
import Log (LogLevel, logMessageIO)
import Log.Class (MonadLog (..))

import Control.Monad.IO.Unlift
import Log.Logger (LoggerEnv (..))
import PSR.ConfigMap
import PSR.Storage.Interface

import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import Lens.Micro.Platform qualified as Export (view)
import Log.Class as Export (MonadLog (..), logAttention, logAttention_, logInfo, logInfo_, logTrace, logTrace_)

data AppConfig = AppConfig
    { _confLoggerEnv :: LoggerEnv
    , _confConfigMap :: ConfigMap
    , _confStorage :: Storage
    }

-- class HasAppConfig a where
--      appConfig :: Lens a AppConfig,
--      confLoggerEnv :: Lens a ConfigMap
--      ...
makeClassy ''AppConfig
instance HasConfigMap AppConfig where configMap = confConfigMap

newtype App context err a
    = App {_runApp :: ExceptT err (ReaderT context IO) a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadReader context
        , MonadError err
        )

runApp :: context -> App context err a -> IO (Either err a)
runApp conf = flip runReaderT conf . runExceptT . _runApp

-- TODO: Move into its own module, and use record instead of
class HasLoggerEnv a where
    loggerEnv :: Lens' a LoggerEnv
    loggerEnvData :: Lens' a [Pair]
    loggerEnvData = loggerEnv . lens leData (\lEnv new -> lEnv{leData = new})
    loggerEnvDomain :: Lens' a [Text]
    loggerEnvDomain = loggerEnv . lens leDomain (\lEnv new -> lEnv{leDomain = new})
    loggerEnvMaxLogLevel :: Lens' a LogLevel
    loggerEnvMaxLogLevel = loggerEnv . lens leMaxLogLevel (\lEnv new -> lEnv{leMaxLogLevel = new})

instance HasLoggerEnv LoggerEnv where loggerEnv = id
instance HasLoggerEnv AppConfig where loggerEnv = confLoggerEnv
instance (HasLoggerEnv context) => MonadLog (App context err) where
    getLoggerEnv = view loggerEnv

    localData pairs = local (loggerEnvData %~ (++ pairs))
    localDomain dom = local (loggerEnvDomain %~ (++ [dom]))
    localMaxLogLevel level = local (loggerEnvMaxLogLevel .~ level)

    logMessage level message data_ = App . ExceptT . fmap Right . ReaderT $ \context -> do
        time <- getCurrentTime
        logMessageIO (context ^. loggerEnv) time level message data_

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data ChainSyncEvent
    = RollForward C.BlockInMode C.ChainTip
    | RollBackward C.ChainPoint C.ChainTip
    deriving stock (Show, Generic)

isByron :: ChainSyncEvent -> Bool
isByron (RollForward (C.BlockInMode C.ByronEra _) _) = True
isByron _ = False

data ChainSyncEventException = NoIntersectionFound
    deriving stock (Show)
    deriving anyclass (Exception)

data Block where
    Block :: C.ShelleyBasedEra era -> [C.Tx era] -> Block

deriving instance Show Block
