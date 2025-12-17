{-# LANGUAGE TemplateHaskell #-}

module PSR.Types (
    App (..),
    runApp,
    module Export,
    AppConfig (..),
    confLoggerEnv,
    confConfigMap,
    confStorage,
        ChainSyncEvent (..),
    ChainSyncEventException (..), 
    Block (..),
    isByron,
) where

import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Lens.Micro.Platform
import Log (LogLevel, logMessageIO)
import Log.Class (MonadLog (..))

import Control.Monad.IO.Unlift
import Log.Logger (LoggerEnv (..))
import PSR.ConfigMap
import PSR.Storage.Interface

import Lens.Micro.Platform qualified as Export (view)
import Log.Class as Export (MonadLog (..), logAttention, logAttention_, logInfo, logInfo_, logTrace, logTrace_)

data AppConfig = AppConfig
    { _confLoggerEnv :: LoggerEnv
    , _confConfigMap :: ConfigMap
    , _confStorage :: Storage
    }

makeLenses ''AppConfig
leDataL :: ASetter' LoggerEnv [Pair]
leDataL = lens leData (\loggerEnv new -> loggerEnv{leData = new})

leDomainL :: ASetter' LoggerEnv [Text]
leDomainL = lens leDomain (\loggerEnv new -> loggerEnv{leDomain = new})

leMaxLogLevelL :: ASetter' LoggerEnv LogLevel
leMaxLogLevelL = lens leMaxLogLevel (\loggerEnv new -> loggerEnv{leMaxLogLevel = new})

newtype App a
    = App {_runApp :: ReaderT AppConfig IO a}
    deriving newtype (Functor, Applicative, Monad, MonadReader AppConfig, MonadIO, MonadUnliftIO)

runApp :: AppConfig -> App a -> IO a
runApp conf = flip runReaderT conf . _runApp

instance MonadLog App where
    getLoggerEnv = view confLoggerEnv

    localData pairs = local (confLoggerEnv . leDataL %~ (++ pairs))
    localDomain dom = local (confLoggerEnv . leDomainL %~ (++ [dom]))
    localMaxLogLevel level = local (confLoggerEnv . leMaxLogLevelL .~ level)

    logMessage level message data_ = App . ReaderT $ \appEnv -> do
        time <- getCurrentTime
        logMessageIO (_confLoggerEnv appEnv) time level message data_


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Exception (Exception)
import GHC.Generics (Generic)

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
