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
import PlutusLedgerApi.Common

-- | Represents the config map file on disk
data ConfigMapFile = ConfigMapFile
    { cmfStart :: Maybe C.ChainPoint
    , cmfScripts :: [ScriptDetails]
    }
    deriving (Show, Eq)

instance FromJSON ConfigMapFile where
    parseJSON = withObject "ConfigMapFile" $ \v ->
        ConfigMapFile
            <$> v .: "start"
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
    = CBORHex JSONEncodedScript
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
    }

-- | Information relating to a loaded script
data ResolvedScript = ResolvedScript
    { rsScriptHash :: PolicyId
    , rsName :: Maybe Text
    , rsSource :: Maybe ScriptSource
    , -- TODO: Should this be incorporated into rsScourse?
      --       Can't be Just when rsSource is Nothing
      rsScriptFileContent :: Maybe JSONEncodedScript
    }
    deriving (Show, Eq)

{- | Plutus script encoded in JSON (or Yaml) including its version and
  hex encoded script
-}
data JSONEncodedScript = JSONEncodedScript
    { jesType :: AnyPlutusScriptVersion
    , jesDescription :: Maybe Text
    , jesCborHex :: SerialisedScript
    }
    deriving (Show, Eq)

instance FromJSON JSONEncodedScript where
    parseJSON = withObject "JSONEncodedScript" $ \v ->
        JSONEncodedScript
            <$> v .: "type"
            <*> v .:? "description"
            <*> (parseSerialisedScript =<< (v .: "cborHex"))

instance ToJSON JSONEncodedScript where
    toJSON (JSONEncodedScript ty desc scr) =
        object $
            concat
                [ ["type" .= ty]
                , maybe [] ((: []) . ("description" .=)) desc
                , ["cborHex" .= serialisedScriptToText scr]
                ]

{- | Convert a Text containing hex encoded script into a (possible invalid)
'SerialisedScript'
-}
parseSerialisedScript :: Text -> Parser SerialisedScript
parseSerialisedScript =
    either fail (pure . toShort) . convertFromBase Base16 . encodeUtf8

-- | Encode a SerialisedScript into Text
serialisedScriptToText :: SerialisedScript -> Text
serialisedScriptToText = decodeUtf8 . convertToBase Base16 . fromShort

-- | Resolve a script, either from disk or inline definition
readScriptFile :: ScriptDetails -> ExceptT String IO ResolvedScript
readScriptFile ScriptDetails{..} = do
    rsScriptFileContent <- for sdSource $ \case
        FromFile path -> do
            liftIO $ putStrLn $ "Reading script from file: " <> path
            modifyError show $ ExceptT $ decodeFileEither path
        CBORHex content -> pure content

    pure
        ResolvedScript
            { rsScriptHash = sdScriptHash
            , rsName = sdName
            , rsSource = sdSource
            , rsScriptFileContent
            }

-- | Parse the config from a given Yaml file on disk
readConfigMap :: FilePath -> IO (Either String ConfigMap)
readConfigMap path = runExceptT $ do
    ConfigMapFile{..} <- modifyError show $ ExceptT $ decodeFileEither path
    scripts' <- mapM readScriptFile cmfScripts
    pure
        ConfigMap
            { cmStart = cmfStart
            , cmScripts = Map.fromList [(rsScriptHash x, x) | x <- scripts']
            }
