{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Export where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api (
    File (..),
    IsPlutusScriptLanguage,
    PlutusScript,
    writeFileTextEnvelope,
 )
import Cardano.Api.Plutus (PlutusScript (..))
import Onchain.Simple (CompiledCodeLang (..))
import PlutusLedgerApi.Common (serialiseCompiledCode)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

writePlutusScript ::
    forall lang a.
    (IsPlutusScriptLanguage lang) =>
    FilePath -> CompiledCodeLang lang a -> IO ()
writePlutusScript filename (CompiledCodeLang compiledCode) = do
    let serialisedScript = serialiseCompiledCode compiledCode
        script = PlutusScriptSerialised serialisedScript :: PlutusScript lang
    result <- writeFileTextEnvelope (File filename) Nothing script
    case result of
        Left err -> print err
        Right () -> putStrLn $ "Successfully wrote script to: " ++ filename
