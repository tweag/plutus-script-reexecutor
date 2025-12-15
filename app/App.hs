--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Cardano.Api qualified as C
import Control.Concurrent.Async qualified as Async
import Data.Function ((&))
import Options
import Options.Applicative
import PSR.Chain
import PSR.ConfigMap qualified as CM
import PSR.ContextBuilder
import PSR.HTTP qualified as HTTP
import PSR.Streaming
import Streamly.Data.Fold.Prelude qualified as Fold
import Streamly.Data.Stream.Prelude qualified as Stream
import Text.Pretty.Simple

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

compactPrintOpts :: OutputOptions
compactPrintOpts =
    defaultOutputOptionsDarkBg
        { outputOptionsCompact = True
        , outputOptionsCompactParens = True
        , outputOptionsIndentAmount = 2
        , outputOptionsStringStyle = Literal
        }

pCompact :: (Show a) => a -> IO ()
pCompact = pPrintOpt CheckColorTty compactPrintOpts

main :: IO ()
main = do
    Options{..} <- execParser psrOpts
    config@CM.ConfigMap{..} <-
        CM.readConfigMap scriptYaml networkId socketPath >>= either error pure

    let conn = mkLocalNodeConnectInfo networkId socketPath
    start <- maybe (C.chainTipToChainPoint <$> C.getLocalChainTip conn) pure cmStart
    let points = [start]

    -- TODO: Use a logging interface instead of using putStrLn.
    putStrLn "Started..."

    Async.withAsync (HTTP.run httpServerPort) $ \serverAsync -> do
        Async.link serverAsync

        streamChainSyncEvents cmLocalNodeConn points
            & Stream.filter (not . isByron)
            & fmap getEventTransactions
            & Stream.postscanl trackPreviousChainPoint
            -- TODO: Try to replace "concatMap" with "unfoldEach".
            -- TODO: CostModels should probably be requested here instead of per transaction
            & Stream.concatMap (Stream.fromList . (\(a, b) -> (a,) <$> b))
            & Stream.mapM (mkContext1 cmLocalNodeConn . uncurry mkContext0)
            & Stream.mapMaybe (mkContext2 config)
            & Stream.mapMaybeM (mkContext3 config)
            & Stream.trace
                ( \(Context3 (Context2 _ scripts) _ _) -> do
                    -- pCompact ctx
                    putStrLn "Found scripts:"
                    mapM_ pCompact scripts
                )
            & Stream.fold Fold.drain
