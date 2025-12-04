--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Yaml (decodeFileThrow)
import Options
import Options.Applicative
import PSR.Chain
import PSR.ConfigMap qualified as CM
import PSR.ContextBuilder
import PSR.Streaming
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import System.Exit (exitFailure)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    CM.ConfigMap{..} <- decodeFileThrow scriptYaml
    -- TODO: Remove
    -- TODO: Is there really no better way to parse a hash???
    hash <- case C.deserialiseFromJSON "\"082efb838a6a38c435bf5a1c823569ad224fab42034d265b95f88665ee27877f\"" of
        Left err -> do
            print err
            exitFailure
        Right h -> pure h
    let points = maybe [C.ChainPoint 173166134 hash] pure start -- [ChainPointAtGenesis]
    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    let confPolicyMap = Map.fromList [(CM.script_hash x, x) | x <- scripts]
        conn = mkLocalNodeConnectInfo networkId socketPath
    streamChainSyncEvents conn points
        & Stream.filter (not . isByron)
        & fmap getEventTransactions
        & Stream.postscanl trackPreviousChainPoint
        -- TODO: Try to replace "concatMap" with "unfoldEach".
        & Stream.concatMap (Stream.fromList . (\(a, b) -> (a,) <$> b))
        & fmap (uncurry mkContext0)
        -- NOTE: To debug the filter function use filterM along with logging.
        & Stream.filter
            (not . Map.null . Map.restrictKeys confPolicyMap . getMintPolicies)
        & Stream.mapM (mkContext1 conn)
        & Stream.filter
            (not . Map.null . Map.restrictKeys confPolicyMap . getSpendPolicies)
        & Stream.fold (Fold.drainMapM print)
