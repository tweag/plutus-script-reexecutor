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
        & Stream.tap
            ( Fold.drainMapM
                ( \(Context2 ctx scripts) -> do
                    pPrintOpt CheckColorTty compactPrintOpts ctx
                    putStrLn "Found scripts:"
                    mapM_ pPrint scripts
                )
            )
        & Stream.fold Fold.drain
