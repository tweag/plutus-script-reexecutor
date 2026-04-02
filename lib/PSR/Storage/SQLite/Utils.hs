module PSR.Storage.SQLite.Utils (
    openWithPragmas,
    execute,
    query,
    executeNamed,
    queryNamed,
    mkWhereWithParams,
    initSchema,
    -- Sugared queries
    col,
    sqlInsert,
    sqlInsertLax,
    sqlInsertReturning,
) where

import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Foldable (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Database.SQLite.Simple hiding (execute, executeNamed, query, queryNamed)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.ToField (ToField)
import PSR.Metrics (Summary, observeDuration)
import PSR.Storage.SQLite.Instances ()

pragmas :: [Query]
pragmas =
    [ "synchronous = NORMAL"
    , "journal_mode = WAL"
    , "locking_mode = NORMAL"
    ]

openWithPragmas :: FilePath -> IO Connection
openWithPragmas dbPath = do
    conn <- open dbPath
    forM_ pragmas $ \q -> do
        execute_ conn ("PRAGMA " <> q)
    pure conn

execute :: forall q. (ToRow q) => Summary -> Connection -> Query -> q -> IO ()
execute metric conn q row = observeDuration metric (SQL.execute conn q row)

executeNamed :: Summary -> Connection -> Query -> [NamedParam] -> IO ()
executeNamed metric conn q params = observeDuration metric (SQL.executeNamed conn q params)

query :: forall q r. (ToRow q, FromRow r) => Summary -> Connection -> Query -> q -> IO [r]
query metric conn q params = observeDuration metric (SQL.query conn q params)

queryNamed :: forall r. (FromRow r) => Summary -> Connection -> Query -> [NamedParam] -> IO [r]
queryNamed metric conn q params = observeDuration metric (SQL.queryNamed conn q params)

mkWhereWithParams :: [(Text, NamedParam)] -> (Query, [NamedParam])
mkWhereWithParams [] = (Query "", [])
mkWhereWithParams [(q, p)] = (Query $ " WHERE " <> q, [p])
mkWhereWithParams (headParam : rest) = foldl step (mkWhereWithParams [headParam]) rest
  where
    step (rq, ps) (q, p) = (rq <> " AND " <> Query q, p : ps)

initSchema :: Connection -> IO ()
initSchema conn = withTransaction conn $ do
    let schemaQuery = $(embedStringFile =<< makeRelativeToProject "./schema.sql")
    forM_ (T.split (== ';') schemaQuery) $ \q -> do
        execute_ conn (Query q)

-- "col" is a convenience sugar over (:=)
col :: (ToField v) => Text -> v -> NamedParam
col k v = T.cons ':' k := v

getParamKeys :: [NamedParam] -> [Text]
getParamKeys params = [k | (k := _) <- params]

-- NOTES: The SQL generation follows a certain pattern:
--
-- a) The names of params are always the same as the column names.
-- b) The same parameter can't be used in multiple columns.
--
mkInsertQuery :: Text -> [Text] -> [Text] -> Bool -> Query
mkInsertQuery tableName paramKeys returning isLaxInsert = Query rawQueryText
  where
    colNames = map T.tail paramKeys
    joinComma = T.intercalate ", "
    returningText =
        case returning of
            [] -> ""
            xs -> " RETURNING " <> joinComma xs
    ignoringText = if isLaxInsert then " OR IGNORE" else ""
    rawQueryText =
        mconcat
            [ "INSERT"
            , ignoringText
            , " INTO "
            , tableName
            , " "
            , "("
            , joinComma colNames
            , ") "
            , "VALUES ("
            , joinComma paramKeys
            , ")"
            , returningText
            ]

sqlInsert :: Summary -> Connection -> Text -> [NamedParam] -> IO ()
sqlInsert metrics conn tableName params =
    let qry = mkInsertQuery tableName (getParamKeys params) [] False
     in executeNamed metrics conn qry params

sqlInsertLax :: Summary -> Connection -> Text -> [NamedParam] -> IO ()
sqlInsertLax metrics conn tableName params =
    let qry = mkInsertQuery tableName (getParamKeys params) [] True
     in executeNamed metrics conn qry params

sqlInsertReturning ::
    (FromRow r) =>
    Summary -> Connection -> Text -> [NamedParam] -> [Text] -> IO [r]
sqlInsertReturning metrics conn tableName params returning =
    let qry = mkInsertQuery tableName (getParamKeys params) returning False
     in queryNamed metrics conn qry params
