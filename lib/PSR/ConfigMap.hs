{-# LANGUAGE DerivingVia #-}

module PSR.ConfigMap where

import Cardano.Api as C
import Control.Applicative (asum)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.ByteString.Short (fromShort, toShort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Traversable (for)
import Data.Yaml (decodeFileEither, withObject)
import Data.Yaml.Aeson (Parser, Value (Object), object, (.:), (.:?), (.=))
import PSR.Chain (mkLocalNodeConnectInfo)
import PlutusLedgerApi.Common (
    MajorProtocolVersion,
    PlutusLedgerLanguage (..),
    ScriptForEvaluation,
    SerialisedScript,
    deserialiseScript,
    ledgerLanguageIntroducedIn,
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
    { sdScriptHash :: C.PolicyId
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
    = CBORHex TextEnvelope
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
    , cmScripts :: Map PolicyId ResolvedScript
    , cmLocalNodeConn :: LocalNodeConnectInfo
    }

-- | Information relating to a loaded script
data ResolvedScript = ResolvedScript
    { rsScriptHash :: PolicyId
    , rsName :: Maybe Text
    , rsSource :: Maybe ScriptSource
    , -- TODO: Should this be incorporated into rsScourse?
      --       Can't be Just when rsSource is Nothing
      rsScriptFileContent :: Maybe ScriptInAnyLang
    , rsScriptForEvaluation :: Maybe (ScriptEvaluationParameters, ScriptForEvaluation)
    }
    deriving (Show, Eq)

data ScriptEvaluationParameters where
    ScriptEvaluationParameters ::
        { sepLanguage :: PlutusLedgerLanguage
        , sepProtocolVersion :: MajorProtocolVersion
        } ->
        ScriptEvaluationParameters
    deriving (Show, Eq)

{- | Convert a Text containing hex encoded script into a (possible invalid)
'SerialisedScript'
-}
parseSerialisedScript :: Text -> Parser SerialisedScript
parseSerialisedScript =
    either fail (pure . toShort) . convertFromBase Base16 . encodeUtf8

-- | Encode a SerialisedScript into Text
serialisedScriptToText :: SerialisedScript -> Text
serialisedScriptToText = decodeUtf8 . convertToBase Base16 . fromShort

-- resolveScript :: ScriptInAnyLang -> _
resolveScript :: ScriptInAnyLang -> ExceptT String IO (ScriptEvaluationParameters, ScriptForEvaluation)
resolveScript (scr :: ScriptInAnyLang) = do
    -- TODO: Is this the right protocol major version?
    (lang, protocol, script) <- case scr of
        ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) (PlutusScript _ (PlutusScriptSerialised src)) ->
            pure (PlutusV1, ledgerLanguageIntroducedIn PlutusV1, src)
        ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV2) (PlutusScript _ (PlutusScriptSerialised src)) ->
            pure (PlutusV2, ledgerLanguageIntroducedIn PlutusV2, src)
        ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV3) (PlutusScript _ (PlutusScriptSerialised src)) ->
            pure (PlutusV3, ledgerLanguageIntroducedIn PlutusV3, src)
        _ -> throwError $ "Unsupported script: " ++ show scr

    (ScriptEvaluationParameters lang protocol,)
        <$> modifyError show (deserialiseScript lang protocol script)

-- | Resolve a script, either from disk or inline definition
readScriptFile :: ScriptDetails -> ExceptT String IO ResolvedScript
readScriptFile ScriptDetails{..} = do
    let someTypeFor x v = FromSomeType x (ScriptInAnyLang (PlutusScriptLanguage v) . PlutusScript v)
        v1 = someTypeFor (AsPlutusScript AsPlutusScriptV1) PlutusScriptV1
        v2 = someTypeFor (AsPlutusScript AsPlutusScriptV2) PlutusScriptV2
        v3 = someTypeFor (AsPlutusScript AsPlutusScriptV3) PlutusScriptV3
        scriptTypes = [v1, v2, v3]

    rsScriptFileContent <- for sdSource $ \case
        FromFile path -> do
            liftIO $ putStrLn $ "Reading script from file: " <> path
            modifyError show $ ExceptT $ readFileTextEnvelopeAnyOf scriptTypes (File path)
        CBORHex content ->
            modifyError show $ liftEither $ deserialiseFromTextEnvelopeAnyOf scriptTypes content

    rsScriptForEvaluation <- traverse resolveScript rsScriptFileContent

    pure
        ResolvedScript
            { rsScriptHash = sdScriptHash
            , rsName = sdName
            , rsSource = sdSource
            , rsScriptFileContent
            , rsScriptForEvaluation
            }

{- | Parse the config from a given Yaml file on disk
readConfigMap :: FilePath -> IO (Either String ConfigMap)
-}
readConfigMap :: FilePath -> NetworkId -> SocketPath -> IO (Either String ConfigMap)
readConfigMap scriptYaml networkId socketPath = runExceptT $ do
    ConfigMapFile{..} <- modifyError show $ ExceptT $ decodeFileEither scriptYaml
    scripts' <- mapM readScriptFile cmfScripts
    pure
        ConfigMap
            { cmStart = cmfStart
            , cmScripts = Map.fromList [(rsScriptHash x, x) | x <- scripts']
            , cmLocalNodeConn = mkLocalNodeConnectInfo networkId socketPath
            }
