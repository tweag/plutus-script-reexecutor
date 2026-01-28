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
    getPolicyId,
    Wallet (..),
    mkWallet,
    walletKeyHash,
    buildStakeAddress,
    genRegCertStakeAddress,
    genDeregCertStakeAddress,
    -- Globals
    env_LOCAL_CONFIG_DIR,
    env_POPULATE_WORK_DIR,
    env_TESTNET_WORK_DIR,
    env_CARDANO_TESTNET_MAGIC,
    env_CARDANO_TESTNET_NUM_NODES,
    -- Main
    testScriptTrigger,
    escrow,
) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Function ((&))
import Data.Word (Word8)
import Streamly.Data.Array (Array)
import Streamly.Data.Fold qualified as Fold
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.System.Command qualified as Cmd
import Streamly.Unicode.Stream qualified as Unicode
import Streamly.Unicode.String (str)
import System.FilePath ((<.>), (</>))

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

{- HLINT ignore "Use camelCase" -}

env_POPULATE_WORK_DIR :: FilePath
env_POPULATE_WORK_DIR = "work"

env_TESTNET_WORK_DIR :: FilePath
env_TESTNET_WORK_DIR = "devnet-env"

env_LOCAL_CONFIG_DIR :: FilePath
env_LOCAL_CONFIG_DIR = "local-config"

env_CARDANO_SOCKET :: FilePath
env_CARDANO_SOCKET = env_TESTNET_WORK_DIR </> "socket/node1/sock"

env_CARDANO_TESTNET_MAGIC :: Int
env_CARDANO_TESTNET_MAGIC = 42

env_CARDANO_TESTNET_NUM_NODES :: Int
env_CARDANO_TESTNET_NUM_NODES = 1

env_FAUCET_WALLET_VKEY_FILE :: FilePath
env_FAUCET_WALLET_VKEY_FILE = env_TESTNET_WORK_DIR </> "utxo-keys/utxo1/utxo.vkey"

env_FAUCET_WALLET_SKEY_FILE :: FilePath
env_FAUCET_WALLET_SKEY_FILE = env_TESTNET_WORK_DIR </> "utxo-keys/utxo1/utxo.skey"

env_FAUCET_WALLET_ADDR :: IO String
env_FAUCET_WALLET_ADDR =
    readFile $ env_TESTNET_WORK_DIR </> "utxo-keys/utxo1/utxo.addr"

env_FAUCET_WALLET :: IO Wallet
env_FAUCET_WALLET =
    Wallet env_FAUCET_WALLET_VKEY_FILE env_FAUCET_WALLET_SKEY_FILE
        <$> env_FAUCET_WALLET_ADDR

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
getAddress vkeyFile =
    runCmd
        "cardano-cli conway address build"
        [ optNetwork
        , opt "payment-verification-key-file" vkeyFile
        ]
        & firstNonEmptyLine "getAddress"

getScriptAddress :: FilePath -> IO String
getScriptAddress scriptFile =
    runCmd
        "cardano-cli conway address build"
        [ optNetwork
        , opt "payment-script-file" scriptFile
        ]
        & firstNonEmptyLine "getScriptAddress"

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

buildStakeAddress :: [CmdOption] -> IO ()
buildStakeAddress args =
    runCmd
        "cardano-cli conway stake-address build"
        (optNetwork : args)
        & drain

genRegCertStakeAddress :: [CmdOption] -> IO ()
genRegCertStakeAddress args =
    runCmd "cardano-cli conway stake-address registration-certificate" args
        & drain

genDeregCertStakeAddress :: [CmdOption] -> IO ()
genDeregCertStakeAddress args =
    runCmd "cardano-cli conway stake-address deregistration-certificate" args
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

getUtxoListAt :: String -> IO [String]
getUtxoListAt walletAddr =
    runCmd
        "cardano-cli conway query utxo"
        [ optNetwork
        , optSocketPath
        , opt "address" walletAddr
        ]
        & Cmd.pipeChunks [str|jq -r "keys[]"|]
        & nonEmptyLines
        & Stream.fold Fold.toList

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

keygen :: FilePath -> FilePath -> IO ()
keygen vkey skey =
    runCmd
        "cardano-cli address key-gen"
        [ opt "verification-key-file" vkey
        , opt "signing-key-file" skey
        ]
        & drain

data Wallet
    = Wallet
    { wVKeyFile :: FilePath
    , wSKeyFile :: FilePath
    , wAddress :: String
    }

mkWallet :: FilePath -> String -> IO Wallet
mkWallet dir name = do
    let vkey = dir </> name <.> "vkey"
        skey = dir </> name <.> "skey"
    keygen vkey skey
    addr <- getAddress vkey
    pure $ Wallet vkey skey addr

walletKeyHash :: Wallet -> IO String
walletKeyHash Wallet{..} =
    runCmd
        "cardano-cli address key-hash"
        [ opt "payment-verification-key-file" wVKeyFile
        ]
        & firstNonEmptyLine "walletKeyHash"

fetchWallet :: FilePath -> String -> IO Wallet
fetchWallet dir name = do
    let vkey = dir </> name <.> "vkey"
        skey = dir </> name <.> "skey"
    addr <- getAddress vkey
    pure $ Wallet vkey skey addr

--------------------------------------------------------------------------------
-- Complex Utils
--------------------------------------------------------------------------------

transferAda :: Wallet -> Wallet -> Int -> IO String
transferAda (Wallet _ inSign inAddr) (Wallet _ outSign outAddr) adaToTransfer = do
    ensureBlankWorkDir
    utxoList <- getUtxoListAt inAddr
    let txInList = opt "tx-in" <$> utxoList
        adaStr = show adaToTransfer
    buildTransaction . (txInList ++) $
        [ opt "tx-out" [str|#{outAddr} + #{adaStr}|]
        , opt "change-address" inAddr
        , opt "out-file" env_TX_UNSIGNED
        ]
    signTransaction
        [ opt "signing-key-file" inSign
        , opt "signing-key-file" outSign
        , opt "tx-body-file" env_TX_UNSIGNED
        , opt "out-file" env_TX_SIGNED
        ]
    txId <- getTransactionId env_TX_SIGNED
    printVar "transferAda.txId" txId
    submitTransaction
        [ opt "tx-file" env_TX_SIGNED
        ]
    waitTillExists $ fstOutput txId
    pure txId

--------------------------------------------------------------------------------
-- Test script trigger
--------------------------------------------------------------------------------

data AppEnv = AppEnv
    { validatorAddress :: String
    , assetClass :: String
    , faucetAddr :: String
    , policyFilePath :: FilePath
    , validatorFilePath :: FilePath
    , stakeAddrFilePath :: FilePath
    , regCertFilePath :: FilePath
    , deregCertFilePath :: FilePath
    , numIterations :: Int
    , assetAmount :: String
    }

makeAppEnv :: String -> IO AppEnv
makeAppEnv scriptsDirName = do
    let scriptsDirPath = env_LOCAL_CONFIG_DIR </> scriptsDirName
        policyFilePath = scriptsDirPath </> "policy.plutus"
        validatorFilePath = scriptsDirPath </> "validator.plutus"
        stakeAddrFilePath = scriptsDirPath </> "script.stake.addr"
        regCertFilePath = scriptsDirPath </> "registration.cert"
        deregCertFilePath = scriptsDirPath </> "deregistration.cert"
        tokenName = "TEST_TOKEN"
        assetAmount = "100"
        numIterations = 10

    policyId <- getPolicyId policyFilePath
    faucetAddr <- env_FAUCET_WALLET_ADDR
    tokenNameHex <- hexify tokenName
    validatorAddress <- getScriptAddress validatorFilePath
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
            , stakeAddrFilePath = stakeAddrFilePath
            , regCertFilePath = regCertFilePath
            , deregCertFilePath = deregCertFilePath
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
    waitTillExists $ fstOutput mintTx
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
    waitTillExists $ fstOutput spendTx
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
    waitTillExists $ fstOutput burnTx
    printVar "burnTx" burnTx

runCertifyReg :: AppEnv -> IO ()
runCertifyReg AppEnv{..} = do
    printStep "Certify - Reg"

    faucetUtxo <- getFirstUtxoAt faucetAddr
    buildTransaction
        [ opt "tx-in" faucetUtxo
        , opt "tx-in-collateral" faucetUtxo
        , opt "change-address" faucetAddr
        , opt "certificate-file" regCertFilePath
        , opt "certificate-script-file" policyFilePath
        , opt "certificate-redeemer-value" (7 :: Int)
        , opt "out-file" env_TX_UNSIGNED
        ]
    finalizeCurrentTransaction
    txId <- getTransactionId env_TX_SIGNED
    waitTillExists $ fstOutput txId
    printVar "runCertifyReg.txId" txId

runCertifyDereg :: AppEnv -> IO ()
runCertifyDereg AppEnv{..} = do
    printStep "Certify - Dereg"

    faucetUtxo <- getFirstUtxoAt faucetAddr
    buildTransaction
        [ opt "tx-in" faucetUtxo
        , opt "tx-in-collateral" faucetUtxo
        , opt "change-address" faucetAddr
        , opt "certificate-file" deregCertFilePath
        , opt "certificate-script-file" policyFilePath
        , opt "certificate-redeemer-value" (7 :: Int)
        , opt "out-file" env_TX_UNSIGNED
        ]
    finalizeCurrentTransaction
    txId <- getTransactionId env_TX_SIGNED
    waitTillExists $ fstOutput txId
    printVar "runCertifyDereg.txId" txId

runReward :: AppEnv -> IO ()
runReward AppEnv{..} = do
    printStep "Reward"

    faucetUtxo <- getFirstUtxoAt faucetAddr
    stakeAddr <- readFile stakeAddrFilePath
    buildTransaction
        [ opt "tx-in" faucetUtxo
        , opt "tx-in-collateral" faucetUtxo
        , opt "change-address" faucetAddr
        , -- NOTE: +0 is a handy way to trigger the script without involving any
          -- funds.
          opt "withdrawal" [str|#{stakeAddr}+0|]
        , opt "withdrawal-script-file" policyFilePath
        , opt "withdrawal-redeemer-value" (9 :: Int)
        , opt "out-file" env_TX_UNSIGNED
        ]
    finalizeCurrentTransaction
    txId <- getTransactionId env_TX_SIGNED
    waitTillExists $ fstOutput txId
    printVar "runReward.txId" txId

testScriptTriggerWith :: AppEnv -> IO ()
testScriptTriggerWith appEnv = do
    utxo0 <- fstOutput <$> runMint appEnv
    utxo1 <- fstOutput <$> runSpend appEnv utxo0
    runBurn appEnv utxo1
    runCertifyReg appEnv
    runReward appEnv
    runCertifyDereg appEnv

testScriptTrigger :: IO ()
testScriptTrigger = do
    testScriptTriggerWith =<< makeAppEnv "tracing-plutus-v2"
    testScriptTriggerWith =<< makeAppEnv "tracing-plutus-v3"

--------------------------------------------------------------------------------
-- Escrow
--------------------------------------------------------------------------------

-- TODO: Add escrow logic
escrow :: IO ()
escrow = do
    alice <- fetchWallet env_LOCAL_CONFIG_DIR "alice"
    bob <- fetchWallet env_LOCAL_CONFIG_DIR "bob"
    faucet <- env_FAUCET_WALLET
    void $ transferAda faucet alice 2000000
    void $ transferAda faucet bob 2000000
