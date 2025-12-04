{-# LANGUAGE DerivingVia #-}

module PSR.ConfigMap where

import Cardano.Api as C
import GHC.Generics (Generic, Generically (..))

data ConfigMap = ConfigMap
    { start :: Maybe C.ChainPoint
    , scripts :: [ScriptDetails]
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via Generically ConfigMap

data ScriptDetails = ScriptDetails
    { script_hash :: C.PolicyId
    , name :: Maybe String
    }
    deriving (Show, Eq, Generic)
    deriving (ToJSON, FromJSON) via Generically ScriptDetails
