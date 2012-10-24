This module wraps
[postgresql-simple](http://hackage.haskell.org/package/postgresql-simple) to
provide a convenient and composable transactions system.  As a quick example,
one can write code like this:

    {-# LANGUAGE OverloadedStrings #-}
    import Database.PostgreSQL.Simple.Transactions
    import Control.Exception
    
    checkFirst :: PgTx -> IO Int
    checkFirst pg = withTransaction pg mkRow `onException` readRow
      where
      mkRow pg' = do
        [Only r] <- query_ pg' "INSERT INTO T1(a,b) VALUES(1,2) RETURNING id"
        return r
    
      readRow :: IO Int
      readRow = do
        [Only r] <- query_ pg "SELECT id FROM T1 WHERE a=1 AND b=2"
        return r
    
    checkSecond :: PgTx -> IO Int
    checkSecond pg = withTransaction pg mkRow `onException` readRow
      where
      mkRow pg' = do
        [Only r] <- query_ pg' "INSERT INTO T2(a,b) VALUES(1,2) RETURNING id"
        return r
    
      readRow :: IO Int
      readRow = do
        [Only r] <- query_ pg "SELECT id FROM T2 WHERE a=1 AND b=2"
        return r
    
    doBoth :: PgTx -> IO (Int, Int)
    doBoth pg = withTransaction pg checks
      where
      checks pg' = do
        a <- checkFirst pg'
        b <- checkSecond pg'
        return (a,b)
    
    main = do
      c <- connect "dbname=tsuraan"
      doBoth c >>= print

So, basically transactions are composable, which is a bit nicer than having
some functions manually use savepoints, while other functions use
transactions, and only the documentation can tell you which are which.

