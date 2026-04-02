module PSR.Storage.SQLite.Metrics where

import PSR.Metrics (Summary, regSummary)
import PSR.Storage.SQLite.Instances ()

data SqliteMetrics = SqliteMetrics
    { createBlockIfNotExists_insert :: Summary
    , getOrCreateCostModelParamsId_insert :: Summary
    , getOrCreateCostModelParamsId_select :: Summary
    , setOrCreateBlockId_insert :: Summary
    , setOrCreateBlockId_select :: Summary
    , addExecutionEvent_insert :: Summary
    , addRollbackEvent_insert :: Summary
    , addRollbackBlock_insert :: Summary
    , addSelectionEvent_insert :: Summary
    , getEvents_select :: Summary
    , getExecutionContextByNameOrScriptHash_select :: Summary
    }

initialiseMetrics :: IO SqliteMetrics
initialiseMetrics = do
    createBlockIfNotExists_insert <-
        regSummary
            "sqlite_createBlockIfNotExists_insert"
            "Execution time of createBlockIfNotExists insert query"
    getOrCreateCostModelParamsId_select <-
        regSummary
            "sqlite_getOrCreateCostModelParamsId_select"
            "Execution time of getOrCreateCostModelParamsId select query"
    getOrCreateCostModelParamsId_insert <-
        regSummary
            "sqlite_getOrCreateCostModelParamsId_insert"
            "Execution time of getOrCreateCostModelParamsId select query"
    setOrCreateBlockId_insert <-
        regSummary
            "sqlite_setOrCreateBlockId_insert"
            "Execution time of setOrCreateBlockId insert query"
    setOrCreateBlockId_select <-
        regSummary
            "sqlite_setOrCreateBlockId_select"
            "Execution time of setOrCreateBlockId select query"
    addExecutionEvent_insert <-
        regSummary
            "sqlite_addExecutionEvent_insert"
            "Execution time of addExecutionEvent insert query"
    addRollbackEvent_insert <-
        regSummary
            "sqlite_addRollbackEvent_insert"
            "Execution time of addRollbackEvent insert query"
    addRollbackBlock_insert <-
        regSummary
            "sqlite_addRollbackBlock_insert"
            "Execution time of addRollbackEvent block insert query"
    addSelectionEvent_insert <-
        regSummary
            "sqlite_addSelectionEvent_insert"
            "Execution time of addSelectionEvent insert query"
    getEvents_select <-
        regSummary
            "sqlite_getEvents_select"
            "Execution time of getEvents select query"
    getExecutionContextByNameOrScriptHash_select <-
        regSummary
            "sqlite_getOrCreateCostModelParamsId_select"
            "Execution time of getExecutionContextByNameOrScriptHash select query"
    pure SqliteMetrics{..}
