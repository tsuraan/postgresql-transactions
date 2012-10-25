This module wraps
[postgresql-simple](http://hackage.haskell.org/package/postgresql-simple) to
provide a convenient and composable transactions system.  As a quick example,
one can write code like this:

    {-# LANGUAGE OverloadedStrings #-}
    {-|
      This is a simple motivating example for why postgresql-transactions may be
      a useful thing to have.  Prepare the database with the following:
    
      BEGIN;
      CREATE TABLE Users( userid SERIAL NOT NULL PRIMARY KEY
                        , username TEXT NOT NULL UNIQUE);
    
      CREATE TABLE Messages( msgid SERIAL NOT NULL PRIMARY KEY
                           , msg TEXT NOT NULL UNIQUE);
    
      CREATE TABLE Logs( logid SERIAL NOT NULL PRIMARY KEY
                       , logged TIMESTAMP NOT NULL DEFAULT now()
                       , userid INT NOT NULL REFERENCES Users
                       , msgid INT NOT NULL REFERENCES Messages
                       );
      COMMIT;
    -}
    
    import Database.PostgreSQL.Simple.Transactions
    import Data.ByteString ( ByteString )
    import Control.Exception
    
    getOrCreate :: PgTx -> Query -> Query -> ByteString -> IO Int
    getOrCreate pg get create value = readRow
      where
      readRow :: IO Int
      readRow = do
        rows <- query pg get (Only value)
        case rows of
          []           -> withTransaction pg mkRow `onException` readRow
          [Only rowid] -> return rowid
    
      mkRow :: PgTx -> IO Int
      mkRow pg' = do
        [Only r] <- query pg' create (Only value)
        return r
    
    getUserId :: PgTx -> ByteString -> IO Int
    getUserId pg =
      getOrCreate pg
                  "SELECT userid FROM Users WHERE username=?"
                  "INSERT INTO Users(username) VALUES(?) RETURNING userid"
    
    getMsgId :: PgTx -> ByteString -> IO Int
    getMsgId pg = 
      getOrCreate pg
                  "SELECT msgid FROM Messages where msg=?"
                  "INSERT INTO Messages(msg) VALUES(?) RETURNING msgid"
    
    logMsg :: PgTx -> ByteString -> ByteString -> IO Int
    logMsg pg username msg = withTransaction pg go
      where
      go :: PgTx -> IO Int
      go pg' = do
        userid <- getUserId pg' username
        msgid  <- getMsgId pg' msg
        [Only logid] <- query pg' "INSERT INTO Logs(userid, msgid) VALUES(?, ?) \
                                  \RETURNING logid"
                                  (userid, msgid)
        return logid
    
    main = do
      c <- connect "host=localhost"
      getUserId c "jim" >>= \jimid -> putStrLn ("jim id is " ++ (show jimid))
      logMsg c "al" "login" >>= print
      logMsg c "bob" "login" >>= print
      logMsg c "al" "read email" >>= print
      logMsg c "al" "logout" >>= print
      logMsg c "bob" "logout" >>= print


So, basically transactions are composable, which is a bit nicer than having
some functions manually use savepoints, while other functions use
transactions, and only the documentation can tell you which are which.

