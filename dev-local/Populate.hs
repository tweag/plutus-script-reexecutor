{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Populate (
    -- Utils
    printStep,
    runCmd_,
    runCmd,
    flg,
    opt,
    drain,
    -- Globals
    env_POPULATE_WORK_DIR,
    env_TESTNET_WORK_DIR,
    env_CARDANO_TESTNET_MAGIC,
    env_CARDANO_TESTNET_NUM_NODES,
    -- Main
    populate,
) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.System.Command qualified as Cmd
import Streamly.Unicode.Stream qualified as Unicode
import Streamly.Unicode.String (str)
import System.FilePath ((</>))

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

printStep :: String -> IO ()
printStep s = putStrLn . unlines $ ["", divider, s, divider]
  where
    divider = replicate 80 '-'

drain :: (Monad m) => Stream m a -> m ()
drain = Stream.fold Fold.drain

nonEmptyLines :: Stream IO (Array Word8) -> Stream IO String
nonEmptyLines inp =
    Unicode.decodeUtf8Chunks inp
        & Stream.foldMany (Fold.takeEndBy_ (== '\n') Fold.toList)
        & Stream.filter (not . null)

firstNonEmptyLine :: String -> Stream IO (Array Word8) -> IO String
firstNonEmptyLine tag =
    Stream.fold (maybe (error [str|Empty: #{tag}|]) id <$> Fold.one)
        . nonEmptyLines

printVar :: String -> String -> IO ()
printVar tag val = putStrLn [str|[#{tag}]: #{val}|]

ensureBlankWorkDir :: IO ()
ensureBlankWorkDir = do
    Cmd.toStdout [str|rm -rf #{env_POPULATE_WORK_DIR}|]
    Cmd.toStdout [str|mkdir -p #{env_POPULATE_WORK_DIR}|]

waitTillExists :: String -> IO ()
waitTillExists utxo =
    Stream.repeatM (printStep "Waiting" >> threadDelay 3000000 >> nullUtxo utxo)
        & Stream.takeWhile id
        & Stream.fold Fold.drain

fstOutput :: String -> String
fstOutput txid = [str|#{txid}#0|]

hexify :: String -> IO String
hexify val =
    Cmd.toChars [str|printf "%s" "#{val}"|]
        & Cmd.pipeChars "xxd -p"
        & Stream.takeWhile (/= '\n')
        & Stream.fold Fold.toList

-------------------------------------------------------------------------------
-- Globals
-------------------------------------------------------------------------------

env_POPULATE_WORK_DIR :: FilePath
env_POPULATE_WORK_DIR = "work"

env_TESTNET_WORK_DIR :: FilePath
env_TESTNET_WORK_DIR = "devnet-env"

env_CARDANO_SOCKET :: FilePath
env_CARDANO_SOCKET = env_TESTNET_WORK_DIR </> "socket/node1/sock"

env_CARDANO_TESTNET_MAGIC :: Int
env_CARDANO_TESTNET_MAGIC = 42

env_CARDANO_TESTNET_NUM_NODES :: Int
env_CARDANO_TESTNET_NUM_NODES = 1

env_FAUCET_WALLET_SKEY_FILE :: FilePath
env_FAUCET_WALLET_SKEY_FILE = env_TESTNET_WORK_DIR </> "utxo-keys/utxo1/utxo.skey"

env_FAUCET_WALLET_ADDR :: IO String
env_FAUCET_WALLET_ADDR =
    readFile $ env_TESTNET_WORK_DIR </> "utxo-keys/utxo1/utxo.addr"

env_TX_UNSIGNED :: String
env_TX_UNSIGNED = env_POPULATE_WORK_DIR </> "tx.unsigned"

env_TX_SIGNED :: String
env_TX_SIGNED = env_POPULATE_WORK_DIR </> "tx.signed"

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

type Command = String
type CmdOption = (String, Maybe String)

opt :: (Show b) => String -> b -> CmdOption
opt a b = (a, Just (quoted b))
  where
    quoted = show

flg :: String -> CmdOption
flg a = (a, Nothing)

optNetwork :: CmdOption
optNetwork = opt "testnet-magic" env_CARDANO_TESTNET_MAGIC

optSocketPath :: CmdOption
optSocketPath = opt "socket-path" env_CARDANO_SOCKET

runCmd_ :: String -> IO ()
runCmd_ cmd = do
    putStrLn [str|> #{cmd}|]
    Cmd.toStdout cmd

runCmd :: Command -> [CmdOption] -> Stream IO (Array Word8)
runCmd cmd args =
    Stream.before (putStrLn [str|> #{cmdStr}|]) (Cmd.toChunks cmdStr)
  where
    cmdList = cmd : concatMap (\(k, v) -> ["--" ++ k, maybe "" id v]) args
    cmdStr = unwords cmdList

getPolicyId :: FilePath -> IO String
getPolicyId scriptFile =
    runCmd
        "cardano-cli conway transaction policyid"
        [opt "script-file" scriptFile]
        & firstNonEmptyLine "getPolicyId"

getAddress :: FilePath -> IO String
getAddress scriptFile =
    runCmd
        "cardano-cli conway address build"
        [ optNetwork
        , opt "payment-script-file" scriptFile
        ]
        & firstNonEmptyLine "getAddress"

buildTransaction :: [CmdOption] -> IO ()
buildTransaction args =
    runCmd
        "cardano-cli conway transaction build"
        (optNetwork : optSocketPath : args)
        & drain

signTransaction :: [CmdOption] -> IO ()
signTransaction args =
    runCmd
        "cardano-cli conway transaction sign"
        (optNetwork : args)
        & drain

submitTransaction :: [CmdOption] -> IO ()
submitTransaction args =
    runCmd
        "cardano-cli conway transaction submit"
        (optNetwork : optSocketPath : args)
        & drain

getTransactionId :: String -> IO String
getTransactionId txSigned =
    runCmd
        "cardano-cli conway transaction txid"
        [ opt "tx-body-file" txSigned
        ]
        & Cmd.pipeChunks [str|jq -r ".txhash"|]
        & firstNonEmptyLine "getTransactionId"

getFirstUtxoAt :: String -> IO String
getFirstUtxoAt walletAddr =
    runCmd
        "cardano-cli conway query utxo"
        [ optNetwork
        , optSocketPath
        , opt "address" walletAddr
        ]
        & Cmd.pipeChunks [str|jq -r "keys[0]"|]
        & firstNonEmptyLine "getFirstUtxoAt"

nullUtxo :: String -> IO Bool
nullUtxo utxo =
    runCmd
        "cardano-cli latest query utxo"
        [ optNetwork
        , optSocketPath
        , opt "tx-in" utxo
        ]
        & Cmd.pipeChunks [str|jq 'type == "object" and length == 0'|]
        & firstNonEmptyLine "nullUtxo"
        & fmap (== "true")

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

data AppEnv = AppEnv
    { validatorAddress :: String
    , assetClass :: String
    , faucetAddr :: String
    , policyFilePath :: FilePath
    , validatorFilePath :: FilePath
    , numIterations :: Int
    , assetAmount :: String
    }

makeAppEnv :: IO AppEnv
makeAppEnv = do
    let policyFilePath = "dev-local/policy.plutus"
        validatorFilePath = "dev-local/validator.plutus"
        tokenName = "TEST_TOKEN"
        assetAmount = "100"
        numIterations = 10

    policyId <- getPolicyId policyFilePath
    faucetAddr <- env_FAUCET_WALLET_ADDR
    tokenNameHex <- hexify tokenName
    validatorAddress <- getAddress validatorFilePath
    let assetClass = [str|#{policyId}.#{tokenNameHex}|]

    printVar "faucetAddr" faucetAddr
    printVar "validatorAddress" validatorAddress

    pure $
        AppEnv
            { validatorAddress = validatorAddress
            , assetClass = assetClass
            , faucetAddr = faucetAddr
            , policyFilePath = policyFilePath
            , validatorFilePath = validatorFilePath
            , assetAmount = assetAmount
            , numIterations = numIterations
            }

finalizeCurrentTransaction :: IO ()
finalizeCurrentTransaction = do
    signTransaction
        [ opt "signing-key-file" env_FAUCET_WALLET_SKEY_FILE
        , opt "tx-body-file" env_TX_UNSIGNED
        , opt "out-file" env_TX_SIGNED
        ]
    submitTransaction
        [ opt "tx-file" env_TX_SIGNED
        ]

runMint :: AppEnv -> IO String
runMint AppEnv{..} = do
    ensureBlankWorkDir
    faucetUtxo <- getFirstUtxoAt faucetAddr
    printStep "Mint"
    buildTransaction
        [ opt "tx-in" faucetUtxo
        , opt "tx-in-collateral" faucetUtxo
        , opt "tx-out" [str|#{validatorAddress} + 2000000 + #{assetAmount} #{assetClass}|]
        , opt "tx-out-inline-datum-value" (10 :: Int)
        , opt "mint" [str|#{assetAmount} #{assetClass}|]
        , opt "mint-script-file" policyFilePath
        , opt "mint-redeemer-value" (5 :: Int)
        , opt "change-address" faucetAddr
        , opt "out-file" env_TX_UNSIGNED
        ]
    finalizeCurrentTransaction
    mintTx <- getTransactionId env_TX_SIGNED
    printVar "mintTx" mintTx
    pure mintTx

runSpend :: AppEnv -> String -> IO String
runSpend AppEnv{..} lockedUtxo = do
    ensureBlankWorkDir

    faucetUtxo <- getFirstUtxoAt faucetAddr
    printVar "faucetUtxo" faucetUtxo

    printStep "Spend"
    buildTransaction
        [ opt "tx-in" faucetUtxo
        , opt "tx-in" lockedUtxo
        , opt "tx-in-script-file" validatorFilePath
        , opt "tx-in-redeemer-value" (10 :: Int)
        , opt "tx-in-collateral" faucetUtxo
        , opt "tx-out" [str|#{validatorAddress} + 2000000 + #{assetAmount} #{assetClass}|]
        , opt "tx-out-inline-datum-value" (20 :: Int)
        , opt "change-address" faucetAddr
        , opt "out-file" env_TX_UNSIGNED
        ]
    finalizeCurrentTransaction
    spendTx <- getTransactionId env_TX_SIGNED
    printVar "spendTx" spendTx
    pure spendTx

runBurn :: AppEnv -> String -> IO ()
runBurn AppEnv{..} lockedUtxo = do
    ensureBlankWorkDir

    faucetUtxo <- getFirstUtxoAt faucetAddr
    printVar "faucetUtxo" faucetUtxo

    printStep "Burn"
    buildTransaction
        [ opt "tx-in" faucetUtxo
        , opt "tx-in" lockedUtxo
        , opt "tx-in-script-file" validatorFilePath
        , opt "tx-in-redeemer-value" (10 :: Int)
        , opt "tx-in-collateral" faucetUtxo
        , opt "mint" [str|-#{assetAmount} #{assetClass}|]
        , opt "mint-script-file" policyFilePath
        , opt "mint-redeemer-value" (5 :: Int)
        , opt "change-address" faucetAddr
        , opt "out-file" env_TX_UNSIGNED
        ]
    finalizeCurrentTransaction
    burnTx <- getTransactionId env_TX_SIGNED
    printVar "burnTx" burnTx

populate :: IO ()
populate = do
    appEnv@AppEnv{numIterations} <- makeAppEnv
    let mint = fstOutput <$> runMint appEnv
        spend u = do
            waitTillExists u
            fstOutput <$> runSpend appEnv u
        burn u = do
            waitTillExists u
            runBurn appEnv u
    Stream.iterateM spend mint
        & Stream.take (numIterations + 1)
        & Stream.fold (Fold.rmapM (maybe (pure ()) burn) Fold.latest)
