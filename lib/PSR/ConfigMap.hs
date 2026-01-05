{-# LANGUAGE DerivingVia #-}

module PSR.ConfigMap (
    ConfigMap (..),
    ResolvedScript (..),
    readConfigMap,
) where

import Cardano.Api qualified as C
import Control.Applicative (asum)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, withExceptT)
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Traversable (for)
import Data.Yaml (decodeFileEither, withObject)
import Data.Yaml.Aeson (Value (Object), object, (.:), (.:?), (.=))
import PSR.Chain (mkLocalNodeConnectInfo)

import PlutusLedgerApi.Common (
    MajorProtocolVersion,
    PlutusLedgerLanguage (..),
 )

-- | Represents the config map file on disk
data ConfigMapFile = ConfigMapFile
    { cmfStart :: Maybe C.ChainPoint
    , cmfScripts :: [ScriptDetails]
    }
    deriving (Show, Eq)

instance FromJSON ConfigMapFile where
    parseJSON = withObject "ConfigMapFile" $ \v ->
        ConfigMapFile
            <$> v .:? "start"
            <*> v .: "scripts"

instance ToJSON ConfigMapFile where
    toJSON (ConfigMapFile strt scrpts) =
        object ["start" .= strt, "scripts" .= scrpts]

-- | Details of each script
data ScriptDetails = ScriptDetails
    { sdScriptHash :: C.ScriptHash
    , sdName :: Maybe Text
    , sdSource :: Maybe ScriptSource
    }
    deriving (Show, Eq)

instance ToJSON ScriptDetails where
    toJSON (ScriptDetails sh nm src) =
        object $
            concat
                [ ["script_hash" .= sh]
                , maybe [] ((: []) . ("name" .=)) nm
                , maybe [] ((: []) . ("source" .=)) src
                ]

instance FromJSON ScriptDetails where
    parseJSON = withObject "ScriptDetails" $ \v ->
        ScriptDetails
            <$> v .: "script_hash"
            <*> v .:? "name"
            <*> v .:? "source"

{- | Where a script's source can be found, either inline in the Yaml file
 or another file on disk.
-}
data ScriptSource
    = CBORHex C.TextEnvelope
    | FromFile FilePath
    deriving (Show, Eq)

instance FromJSON ScriptSource where
    parseJSON = withObject "ScriptSource" $ \v ->
        asum
            [ CBORHex <$> parseJSON (Object v)
            , FromFile <$> v .: "path"
            ]
instance ToJSON ScriptSource where
    toJSON (CBORHex cb) = toJSON cb
    toJSON (FromFile path) = object ["path" .= path]

{- | The resolved configuration map, with any scripts referenced by
  path loaded from disk.
-}
data ConfigMap = ConfigMap
    { cmStart :: Maybe C.ChainPoint
    , cmScripts :: Map C.ScriptHash ResolvedScript
    , cmLocalNodeConn :: C.LocalNodeConnectInfo
    }

-- | Information relating to a loaded script
data ResolvedScript = ResolvedScript
    { rsScriptHash :: C.ScriptHash
    , rsName :: Maybe Text
    , rsSource :: Maybe ScriptSource
    , -- TODO: Should this be incorporated into rsScourse?
      --       Can't be Just when rsSource is Nothing
      rsScriptFileContent :: Maybe C.ScriptInAnyLang
    }
    deriving (Show, Eq)

data ScriptEvaluationParameters where
    ScriptEvaluationParameters ::
        { sepLanguage :: PlutusLedgerLanguage
        , sepProtocolVersion :: MajorProtocolVersion
        } ->
        ScriptEvaluationParameters
    deriving (Show, Eq)

-- | Resolve a script, either from disk or inline definition
readScriptFile :: ScriptDetails -> ExceptT String IO ResolvedScript
readScriptFile ScriptDetails{..} = do
    let someTypeFor x v = C.FromSomeType x (C.ScriptInAnyLang (C.PlutusScriptLanguage v) . C.PlutusScript v)
        v1 = someTypeFor (C.AsPlutusScript C.AsPlutusScriptV1) C.PlutusScriptV1
        v2 = someTypeFor (C.AsPlutusScript C.AsPlutusScriptV2) C.PlutusScriptV2
        v3 = someTypeFor (C.AsPlutusScript C.AsPlutusScriptV3) C.PlutusScriptV3
        scriptTypes = [v1, v2, v3]

    rsScriptFileContent <- for sdSource $ \case
        FromFile path -> do
            liftIO $ putStrLn $ "Reading script from file: " <> path
            withExceptT show $ ExceptT $ C.readFileTextEnvelopeAnyOf scriptTypes (C.File path)
        CBORHex content ->
            withExceptT show $ except $ C.deserialiseFromTextEnvelopeAnyOf scriptTypes content

    pure
        ResolvedScript
            { rsScriptHash = sdScriptHash
            , rsName = sdName
            , rsSource = sdSource
            , rsScriptFileContent
            }

-- | Parse the config from a given Yaml file on disk
readConfigMap :: FilePath -> C.NetworkId -> C.SocketPath -> IO (Either String ConfigMap)
readConfigMap scriptYaml networkId socketPath = runExceptT $ do
    ConfigMapFile{..} <- withExceptT show $ ExceptT $ decodeFileEither scriptYaml
    scripts' <- mapM readScriptFile cmfScripts
    pure
        ConfigMap
            { cmStart = cmfStart
            , cmScripts = Map.fromList [(rsScriptHash x, x) | x <- scripts']
            , cmLocalNodeConn = mkLocalNodeConnectInfo networkId socketPath
            }
