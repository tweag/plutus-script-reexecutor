{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Data.Function ((&))
import Data.String (IsString (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Export (writePlutusScript)
import Onchain.Debug qualified as Debug
import Onchain.Escrow (EscrowParams (..))
import Onchain.Release qualified as Release
import Options.Applicative hiding (str)
import Populate
import Streamly.Console.Stdio qualified as Console
import Streamly.Unicode.String (str)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

-------------------------------------------------------------------------------
-- CLI
-------------------------------------------------------------------------------

data PopulateCommand
    = PCMintSpendBurnLoop
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
            "mint-spend-burn-loop"
            ( info
                (pure PCMintSpendBurnLoop)
                (progDesc "Run the mint, spend, and burn loop scenario")
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

createScriptsYaml :: IO ()
createScriptsYaml = do
    policyPolicyId <- getPolicyId $ env_LOCAL_CONFIG_DIR </> "policy.plutus"
    validatorPolicyId <- getPolicyId $ env_LOCAL_CONFIG_DIR </> "validator.plutus"
    escrowPolicyId <- getPolicyId $ env_LOCAL_CONFIG_DIR </> "escrow.plutus"
    writeFile
        (env_LOCAL_CONFIG_DIR </> "scripts.yaml")
        [str|
scripts:

  - script_hash: "#{policyPolicyId}"
    name: "Local Policy"
    source:
      path: "#{env_LOCAL_CONFIG_DIR}/policy-debug.plutus"

  - script_hash: "#{validatorPolicyId}"
    name: "Local Validator"
    source:
      path: "#{env_LOCAL_CONFIG_DIR}/validator-debug.plutus"

  - script_hash: "#{escrowPolicyId}"
    name: "Escrow"
    source:
      path: "#{env_LOCAL_CONFIG_DIR}/escrow-debug.plutus"
|]

createConfig :: IO ()
createConfig = do
    runCmd_ [str|mkdir -p #{env_LOCAL_CONFIG_DIR}|]
    -- Config for Mint-Spend-Burn Loop
    writePlutusScript (env_LOCAL_CONFIG_DIR </> "policy.plutus") Release.alwaysTrue
    writePlutusScript (env_LOCAL_CONFIG_DIR </> "validator.plutus") Release.alwaysTrue
    writePlutusScript (env_LOCAL_CONFIG_DIR </> "policy-debug.plutus") Debug.alwaysTrue
    writePlutusScript (env_LOCAL_CONFIG_DIR </> "validator-debug.plutus") Debug.alwaysTrue
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
        (Release.escrowValidator escrowParams)
    writePlutusScript
        (env_LOCAL_CONFIG_DIR </> "escrow-debug.plutus")
        (Debug.escrowValidator escrowParams)
    -- Finally create the scripts.yaml file
    createScriptsYaml

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
        Populate PCMintSpendBurnLoop -> mintSpendBurnLoop
        Populate PCEscrow -> escrow
        Setup -> setup
