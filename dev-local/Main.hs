{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Cardano.Api (IsPlutusScriptLanguage)
import Data.Function ((&))
import Data.String (IsString (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Export (writePlutusScript)
import Onchain.V2.Escrow (EscrowParams (..))
import Onchain.V2.Simple (CompiledCodeLang)
import Options.Applicative hiding (str)
import Populate
import Streamly.Console.Stdio qualified as Console
import Streamly.Unicode.String (str)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

import Onchain.V2.Debug qualified as DebugV2
import Onchain.V2.Release qualified as ReleaseV2
import Onchain.V3.Debug qualified as DebugV3
import Onchain.V3.Release qualified as ReleaseV3

-------------------------------------------------------------------------------
-- CLI
-------------------------------------------------------------------------------

data PopulateCommand
    = PCTriggerTest
    | PCEscrow

data Command
    = StartLocalTestnet
    | Clean
    | Populate PopulateCommand
    | Setup

populateCommandParser :: Parser PopulateCommand
populateCommandParser =
    hsubparser
        ( command
            "trigger-test"
            ( info
                (pure PCTriggerTest)
                (progDesc "Run the trigger test")
            )
            <> command
                "escrow"
                ( info
                    (pure PCEscrow)
                    (progDesc "Run the escrow scenario")
                )
        )
commandParser :: Parser Command
commandParser =
    hsubparser
        ( command
            "start-local-testnet"
            ( info
                (pure StartLocalTestnet)
                (progDesc "Start the local development network")
            )
            <> command
                "clean"
                ( info
                    (pure Clean)
                    (progDesc "Clean all local state")
                )
            <> command
                "populate"
                ( info
                    (Populate <$> populateCommandParser)
                    (progDesc "Populate the local network with initial data")
                )
            <> command
                "setup"
                ( info
                    (pure Setup)
                    (progDesc "Setup the initial config files")
                )
        )

opts :: ParserInfo Command
opts =
    info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Local development management tool"
        )

--------------------------------------------------------------------------------
-- Create config
--------------------------------------------------------------------------------

-- TODO: Abstract escrow into v2 and v3.

tracingYamlContents :: String -> IO String
tracingYamlContents scriptsDirName = do
    let scriptsDirPath = env_LOCAL_CONFIG_DIR </> scriptsDirName

    policyPolicyId <- getPolicyId $ scriptsDirPath </> "policy.plutus"
    validatorPolicyId <- getPolicyId $ scriptsDirPath </> "validator.plutus"

    pure
        [str|
  - script_hash: "#{policyPolicyId}"
    name: "Local Policy"
    source:
      path: "#{scriptsDirName}/policy-debug.plutus"

  - script_hash: "#{validatorPolicyId}"
    name: "Local Validator"
    source:
      path: "#{scriptsDirName}/validator-debug.plutus"
|]

escrowYamlContents :: IO String
escrowYamlContents = do
    escrowPolicyId <- getPolicyId $ env_LOCAL_CONFIG_DIR </> "escrow.plutus"
    pure
        [str|
  - script_hash: "#{escrowPolicyId}"
    name: "Escrow"
    source:
      path: "./escrow-debug.plutus"
|]

createScriptsYaml :: IO ()
createScriptsYaml = do
    tracingV2 <- tracingYamlContents "tracing-plutus-v2"
    tracingV3 <- tracingYamlContents "tracing-plutus-v3"
    escrowV2 <- escrowYamlContents
    writeFile
        (env_LOCAL_CONFIG_DIR </> "scripts.yaml")
        [str|
scripts:
#{tracingV2}
#{tracingV3}
#{escrowV2}
|]

createTracingConfig ::
    (IsPlutusScriptLanguage lang) =>
    String ->
    CompiledCodeLang lang pol ->
    CompiledCodeLang lang val ->
    CompiledCodeLang lang pol ->
    CompiledCodeLang lang val ->
    IO ()
createTracingConfig scriptsDirName relP relV debP debV = do
    runCmd_ [str|mkdir -p #{env_LOCAL_CONFIG_DIR}/#{scriptsDirName}|]

    let scriptsDirPath = env_LOCAL_CONFIG_DIR </> scriptsDirName

    writePlutusScript (scriptsDirPath </> "policy.plutus") relP
    writePlutusScript (scriptsDirPath </> "validator.plutus") relV
    writePlutusScript (scriptsDirPath </> "policy-debug.plutus") debP
    writePlutusScript (scriptsDirPath </> "validator-debug.plutus") debV

    buildStakeAddress
        [ opt "stake-script-file" (scriptsDirPath </> "policy.plutus")
        , opt "out-file" (scriptsDirPath </> "script.stake.addr")
        ]
    -- 400_000 here comes from:
    -- cardano-cli conway query protocol-parameters
    --    \ --testnet-magic 42
    --    \ --socket-path "devnet-env/socket/node1/sock"
    --    \ | jq .stakeAddressDeposit
    genRegCertStakeAddress
        [ opt "stake-script-file" (scriptsDirPath </> "policy.plutus")
        , opt "out-file" (scriptsDirPath </> "registration.cert")
        , opt "key-reg-deposit-amt" (400_000 :: Int)
        ]
    genDeregCertStakeAddress
        [ opt "stake-script-file" (scriptsDirPath </> "policy.plutus")
        , opt "out-file" (scriptsDirPath </> "deregistration.cert")
        , opt "key-reg-deposit-amt" (400_000 :: Int)
        ]

createEscrowConfig :: IO ()
createEscrowConfig = do
    -- Config for Escrow
    alice <- mkWallet env_LOCAL_CONFIG_DIR "alice"
    bob <- mkWallet env_LOCAL_CONFIG_DIR "bob"
    alicePkh <- fromString <$> walletKeyHash alice
    bobPkh <- fromString <$> walletKeyHash bob
    now <- getPOSIXTime
    let deadline = floor $ (now + 30) * 1000
    let escrowParams = EscrowParams alicePkh bobPkh deadline
    writePlutusScript
        (env_LOCAL_CONFIG_DIR </> "escrow.plutus")
        (ReleaseV2.escrowValidator escrowParams)
    writePlutusScript
        (env_LOCAL_CONFIG_DIR </> "escrow-debug.plutus")
        (DebugV2.escrowValidator escrowParams)
    -- Finally create the scripts.yaml file
    createScriptsYaml

createConfig :: IO ()
createConfig = do
    runCmd_ [str|mkdir -p #{env_LOCAL_CONFIG_DIR}|]

    createTracingConfig
        "tracing-plutus-v3"
        ReleaseV3.tracingScript
        ReleaseV3.tracingScript
        DebugV3.tracingScript
        DebugV3.tracingScript

    createTracingConfig
        "tracing-plutus-v2"
        ReleaseV2.tracingPolicy
        ReleaseV2.tracingValidator
        DebugV2.tracingPolicy
        DebugV2.tracingValidator

    createEscrowConfig

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

startLocalTestnet :: IO ()
startLocalTestnet = do
    setEnv "CARDANO_CLI" "cardano-cli"
    setEnv "CARDANO_NODE" "cardano-node"
    runCmd
        "cardano-testnet cardano"
        [ opt "num-pool-nodes" env_CARDANO_TESTNET_NUM_NODES
        , flg "conway-era"
        , flg "enable-new-epoch-state-logging"
        , opt "output-dir" env_TESTNET_WORK_DIR
        , opt "testnet-magic" env_CARDANO_TESTNET_MAGIC
        ]
        & Console.putChunks

clean :: IO ()
clean = do
    runCmd_ [str|rm -rf #{env_LOCAL_CONFIG_DIR}|]
    runCmd_ [str|rm -rf #{env_TESTNET_WORK_DIR}|]
    runCmd_ [str|rm -rf #{env_POPULATE_WORK_DIR}|]

setup :: IO ()
setup = do
    clean
    createConfig

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    cmd <- execParser opts
    case cmd of
        StartLocalTestnet -> startLocalTestnet
        Clean -> clean
        Populate PCTriggerTest -> testScriptTrigger
        Populate PCEscrow -> escrow
        Setup -> setup
