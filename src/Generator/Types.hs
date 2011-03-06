module Generator.Types where

import Control.Monad.State

import Language.Rebeca.Absrebeca (Model) 

type Generator = FilePath -> Model -> Result
type TransState = State RebecConfiguration
type ModelCode = String
type RecordCode = String
type MonitorCode = String
type RunCode = String
type Result = (ModelCode, RecordCode, MonitorCode, RunCode)

data RebecConfiguration = RebecConfiguration
    { reactiveClassName :: String
    , envVarNames :: [String]
    , knownRebecNames :: [String]
    , knownRebecRecordName :: Maybe String
    , stateVarNames :: [String]
    , stateVarRecordName :: Maybe String
    , localVarNames :: [String]
    , localVarRecordName :: Maybe String
    , records :: [String]
    }

