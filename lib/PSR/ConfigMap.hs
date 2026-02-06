{-# LANGUAGE DerivingVia #-}

module PSR.ConfigMap (
    ConfigMap (..),
    ResolvedScript (..),
    readConfigMap,
) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Yaml (decodeFileEither)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (LeashID)
import PSR.Chain (mkLocalNodeConnectInfo)
import PSR.Types (deriveJSONRecord, deriveJSONSimpleSum)
import PlutusLedgerApi.Common (
    MajorProtocolVersion,
    PlutusLedgerLanguage (..),
    ScriptForEvaluation,
    deserialiseScript,
    ledgerLanguageIntroducedIn,
 )
import System.Directory (makeRelativeToCurrentDirectory)
import System.FilePath (dropFileName, (</>))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

{- | Where a script's source can be found, either inline in the Yaml file
 or another file on disk.
-}
data ScriptSource
    = SCborHex C.TextEnvelope
    | SFilePath FilePath
    deriving (Show, Eq)

$(deriveJSONSimpleSum "S" ''ScriptSource)

data ScriptDetails = ScriptDetails
    { sdHash :: Text
    , sdSource :: ScriptSource
    , sdName :: Maybe Text
    }
    deriving (Show, Eq)

$(deriveJSONRecord "sd" ''ScriptDetails)

-- | Details of each script
data ScriptSubDetails = ScriptSubDetails
    { sdScriptHash :: C.ScriptHash
    , sdTargetScriptName :: Maybe Text
    , sdSubstitutions :: [ScriptDetails]
    }
    deriving (Show, Eq)

$(deriveJSONRecord "sd" ''ScriptSubDetails)

-- | Represents the config map file on disk
data ConfigMapFile = ConfigMapFile
    { cmfStart :: Maybe C.ChainPoint
    , cmfScripts :: [ScriptSubDetails]
    }
    deriving (Show, Eq)

$(deriveJSONRecord "cmf" ''ConfigMapFile)

{- | The resolved configuration map, with any scripts referenced by
  path loaded from disk.
-}
data ConfigMap = ConfigMap
    { cmStart :: Maybe C.ChainPoint
    , cmScripts :: Map C.ScriptHash [ResolvedScript]
    , cmLocalNodeConn :: C.LocalNodeConnectInfo
    , cmLeashId :: LeashID
    }

-- | Information relating to a loaded script
data ResolvedScript = ResolvedScript
    { rsName :: Maybe Text
    , rsScriptHash :: Text
    , rsScriptFileContent :: C.ScriptInAnyLang
    , rsScriptEvaluationParameters :: ScriptEvaluationParameters
    , rsScriptForEvaluation :: ScriptForEvaluation
    }
    deriving (Show, Eq)

data ScriptEvaluationParameters where
    ScriptEvaluationParameters ::
        { sepLanguage :: PlutusLedgerLanguage
        , sepProtocolVersion :: MajorProtocolVersion
        } ->
        ScriptEvaluationParameters
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------

-- resolveScript :: ScriptInAnyLang -> _
resolveScript :: C.ScriptInAnyLang -> ExceptT String IO (ScriptEvaluationParameters, ScriptForEvaluation)
resolveScript (scr :: C.ScriptInAnyLang) = do
    -- TODO: Is this the right protocol major version?
    (lang, protocol, script) <- case scr of
        C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) (C.PlutusScript _ (C.PlutusScriptSerialised src)) ->
            pure (PlutusV1, ledgerLanguageIntroducedIn PlutusV1, src)
        C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) (C.PlutusScript _ (C.PlutusScriptSerialised src)) ->
            pure (PlutusV2, ledgerLanguageIntroducedIn PlutusV2, src)
        C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) (C.PlutusScript _ (C.PlutusScriptSerialised src)) ->
            pure (PlutusV3, ledgerLanguageIntroducedIn PlutusV3, src)
        _ -> throwE $ "Unsupported script: " ++ show scr

    (ScriptEvaluationParameters lang protocol,)
        <$> withExceptT show (deserialiseScript lang protocol script)

readSubstitutionList :: FilePath -> ScriptSubDetails -> ExceptT String IO (C.ScriptHash, [ResolvedScript])
readSubstitutionList scriptYamlDir ScriptSubDetails{..} = do
    (sdScriptHash,) <$> mapM (readScriptFile scriptYamlDir sdScriptHash) (zip [1 ..] sdSubstitutions)

-- | Resolve a script, either from disk or inline definition
readScriptFile :: FilePath -> C.ScriptHash -> (Int, ScriptDetails) -> ExceptT String IO ResolvedScript
readScriptFile scriptYamlDir scrutScriptHash (ix, ScriptDetails{..}) = do
    let someTypeFor x v = C.FromSomeType x (C.ScriptInAnyLang (C.PlutusScriptLanguage v) . C.PlutusScript v)
        v1 = someTypeFor (C.AsPlutusScript C.AsPlutusScriptV1) C.PlutusScriptV1
        v2 = someTypeFor (C.AsPlutusScript C.AsPlutusScriptV2) C.PlutusScriptV2
        v3 = someTypeFor (C.AsPlutusScript C.AsPlutusScriptV3) C.PlutusScriptV3
        scriptTypes = [v1, v2, v3]

    let errIdentifier =
            Text.concat
                [ "["
                , C.serialiseToRawBytesHexText scrutScriptHash
                , "] at substitution ["
                , Text.pack (show ix)
                , "]"
                ]

    rsScriptFileContent <- case sdSource of
        SFilePath path' -> do
            let path = scriptYamlDir </> path'
            relativePath <- liftIO $ makeRelativeToCurrentDirectory path
            liftIO $ putStrLn $ "Reading script from file: " <> relativePath
            withExceptT show $ ExceptT $ C.readFileTextEnvelopeAnyOf scriptTypes (C.File relativePath)
        SCborHex content ->
            withExceptT show $ except $ C.deserialiseFromTextEnvelopeAnyOf scriptTypes content

    let actualScriptHash =
            case rsScriptFileContent of
                C.ScriptInAnyLang _ script ->
                    C.serialiseToRawBytesHexText $ C.hashScript script

    when (sdHash /= actualScriptHash) $
        fail $
            Text.unpack $
                Text.concat
                    [ "Unable to verify script file at "
                    , errIdentifier
                    , ". Actual hash is: "
                    , actualScriptHash
                    , ", but got "
                    , sdHash
                    ]

    (rsScriptEvaluationParameters, rsScriptForEvaluation) <- resolveScript rsScriptFileContent

    pure
        ResolvedScript
            { rsName = sdName
            , rsScriptHash = sdHash
            , rsScriptFileContent
            , rsScriptEvaluationParameters
            , rsScriptForEvaluation
            }

-- | Parse the config from a given Yaml file on disk
readConfigMap :: FilePath -> C.NetworkId -> C.SocketPath -> LeashID -> IO (Either String ConfigMap)
readConfigMap scriptYaml networkId socketPath leashId = runExceptT $ do
    ConfigMapFile{..} <- withExceptT show $ ExceptT $ decodeFileEither scriptYaml
    let scriptYamlDir = dropFileName scriptYaml
    -- NOTE: The list of substitutions here can be empty, it just signals that
    -- the user is interested in that script and wants to record the
    -- observations.
    --
    -- TODO: Think about what should the behavior look like if the list of
    -- substitutions is empty.
    -- One possible behavior: Evaluate the transaction normally in this case,
    -- and provide the outputs we compute from the real script (like ExUnits).
    --
    kvPairs <- mapM (readSubstitutionList scriptYamlDir) cmfScripts
    pure
        ConfigMap
            { cmStart = cmfStart
            , cmScripts = Map.fromList kvPairs
            , cmLocalNodeConn = mkLocalNodeConnectInfo networkId socketPath
            , cmLeashId = leashId
            }
