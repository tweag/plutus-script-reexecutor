{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Export where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (File (..), IsPlutusScriptLanguage, PlutusScript, PlutusScriptV3, writeFileTextEnvelope)
import Cardano.Api.Plutus (PlutusScript (..))
import Onchain.V2.Simple (CompiledCodeLang (..))
import PlutusLedgerApi.V2 qualified as V2
import PlutusLedgerApi.V3 qualified as V3
import PlutusTx

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

writePlutusScript ::
    forall lang a.
    (IsPlutusScriptLanguage lang) =>
    FilePath ->
    CompiledCodeLang lang a ->
    IO ()
writePlutusScript filename (CompiledCodeLang compiledCode) = do
    let serialisedScript = V2.serialiseCompiledCode compiledCode
        script = PlutusScriptSerialised serialisedScript :: PlutusScript lang
    result <- writeFileTextEnvelope (File filename) Nothing script
    case result of
        Left err -> print err
        Right () -> putStrLn $ "Successfully wrote script to: " ++ filename

writePlutusScriptV3 ::
    forall a.
    FilePath ->
    CompiledCode a ->
    IO ()
writePlutusScriptV3 filename compiledCode = do
    let serialisedScript = V3.serialiseCompiledCode compiledCode
        script = PlutusScriptSerialised serialisedScript :: PlutusScript PlutusScriptV3
    result <- writeFileTextEnvelope (File filename) Nothing script
    case result of
        Left err -> print err
        Right () -> putStrLn $ "Successfully wrote script to: " ++ filename
