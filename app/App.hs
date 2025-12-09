--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Data.Function ((&))
import Options
import Options.Applicative
import PSR.Chain
import PSR.ConfigMap qualified as CM
import PSR.ContextBuilder
import PSR.Streaming
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    config@CM.ConfigMap{..} <- either error pure =<< CM.readConfigMap scriptYaml

    -- TODO: What's a better default here?
    let points = maybe [C.ChainPointAtGenesis] pure cmStart
    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    let conn = mkLocalNodeConnectInfo networkId socketPath
    streamChainSyncEvents conn points
        & Stream.filter (not . isByron)
        & fmap getEventTransactions
        & Stream.postscanl trackPreviousChainPoint
        -- TODO: Try to replace "concatMap" with "unfoldEach".
        & Stream.concatMap (Stream.fromList . (\(a, b) -> (a,) <$> b))
        & Stream.mapM (mkContext1 conn . uncurry mkContext0)
        & Stream.mapMaybe (mkContext2 config)
        & Stream.fold Fold.drain
