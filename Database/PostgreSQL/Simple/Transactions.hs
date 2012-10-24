{-# LANGUAGE OverloadedStrings #-}
module Database.PostgreSQL.Simple.Transactions
( PgTx
, Query  -- re-export for convenience
, Only   -- re-export for convenience
, txConn -- raw connection access for functions I didn't re-implement
, connect
, wrap
, close
, withTransaction
, withTransactionSerializable
, query
, query_
, returning
, execute
, execute_
, executeMany
) where

import qualified Database.PostgreSQL.Simple as S
import qualified Data.ByteString as ByteString
import Control.Applicative ( (<$>) )
import System.Random ( randomRIO )
import Control.Monad ( replicateM )
import Database.PostgreSQL.Simple ( Connection, IsolationLevel(..)
                                  , connectPostgreSQL, Only, Query )
import Data.ByteString ( ByteString )
import Control.Exception ( mask, onException )
import Data.String ( IsString(fromString) )
import Data.Int ( Int64 )

data PgTx = PgTx
  { txDepth :: Int
  , txLevel :: IsolationLevel
  , txConn  :: Connection
  }

connect :: ByteString -> IO PgTx
connect connStr = do
  conn <- S.connectPostgreSQL connStr
  return $ PgTx 0 undefined conn

wrap :: Connection -> PgTx
wrap = PgTx 0 undefined

close :: PgTx -> IO ()
close = S.close . txConn

withTransaction :: PgTx -> (PgTx -> IO a) -> IO a
withTransaction (PgTx depth level conn) act =
  if depth == 0
  then S.withTransaction conn $ act (PgTx (depth+1) ReadCommitted conn)
  else withSavepoint conn $ act (PgTx (depth+1) level conn)

withTransactionSerializable :: PgTx -> (PgTx -> IO a) -> IO a
withTransactionSerializable (PgTx depth level conn) act =
  case (depth, level) of
    (0, _) ->
      S.withTransactionSerializable conn $ act (PgTx (depth+1) Serializable conn)
    (_, Serializable) ->
      withSavepoint conn $ act (PgTx (depth+1) level conn)
    (_, n) -> do
      S.execute_ conn "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"
      withSavepoint conn $ act (PgTx (depth+1) Serializable conn)

query :: (S.ToRow q, S.FromRow r) => PgTx -> S.Query -> q -> IO [r]
query = S.query . txConn

query_ :: S.FromRow r => PgTx -> S.Query -> IO [r]
query_ = S.query_ . txConn

returning :: (S.ToRow q, S.FromRow r) => PgTx -> S.Query -> [q] -> IO [r]
returning = S.returning . txConn

execute :: S.ToRow q => PgTx -> S.Query -> q -> IO Int64
execute = S.execute . txConn

execute_ :: PgTx -> S.Query -> IO Int64
execute_ = S.execute_ . txConn

executeMany :: S.ToRow q => PgTx -> S.Query -> [q] -> IO Int64
executeMany = S.executeMany . txConn

withSavepoint :: Connection -> IO a -> IO a
withSavepoint conn act = do
  spname <- map toEnum <$> replicateM 10 (randomRIO (97, 122))
  let sp = fromString $ "SAVEPOINT " ++ spname
  let rb = fromString $ "ROLLBACK TO SAVEPOINT " ++ spname
  let re = fromString $ "RELEASE SAVEPOINT " ++ spname
  mask $ \restore -> do
    S.execute_ conn sp
    r <- restore act `onException` S.execute_ conn rb
    S.execute_ conn re
    return r

