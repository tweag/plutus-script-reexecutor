module PSR.Metrics (
    regSummary,
    standardQuantiles,
    regCounter,
    regGauge,
    -- Re-exports
    Summary,
    observeDuration,
    observe,
    Counter,
    incCounter,
    incCounterBy,
    Gauge,
    getGauge,
    setGauge,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor (void)
import Data.Text (Text)
import Prometheus

standardQuantiles :: [Quantile]
standardQuantiles = defaultQuantiles ++ [(0.999, 0.001)]

regSummary :: (MonadIO m) => Text -> Text -> m Summary
regSummary name info = register $ summary (Info name info) standardQuantiles

regGauge :: (MonadIO m) => Text -> Text -> m Gauge
regGauge name info = register $ gauge (Info name info)

regCounter :: Text -> Text -> IO Counter
regCounter name info = register $ counter (Info name info)

incCounterBy :: (Integral a) => Counter -> a -> IO ()
incCounterBy c n = void $ addCounter c (fromIntegral n)
