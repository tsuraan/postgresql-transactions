This module wraps
[postgresql-simple](http://hackage.haskell.org/package/postgresql-simple) to
provide a convenient and composable transactions system.  As a quick example,
one can write code like this:

    checkFirst :: PgTx -> IO Int
    checkFirst pg = withTransaction mkRow `onException` readRow
      where
      mkRow pg' = execute_ pg' "INSERT INTO T1(a,b) VALUES(1,2) RETURNING id"
      readRow   = execute_ "SELECT id FROM T1 WHERE a=1 AND b=2"

    checkSecond :: PgTx -> IO Int
    checkSecond pg = withTransaction mkRow `onException` readRow
      where
      mkRow pg' = execute_ pg' "INSERT INTO T2(a,b) VALUES(1,2) RETURNING id"
      readRow   = execute_ "SELECT id FROM T2 WHERE a=1 AND b=2"

    doBoth :: PgTx -> IO (Int, Int)
    doBoth pg = withTransaction pg checks
      where
      checks pg' = do
        a <- checkFirst pg'
        b <- checkSecond pg'
        return (a,b)

So, basically transactions are composable, which is a bit nicer than having
some functions manually use savepoints, while other functions use
transactions, and only the documentation can tell you which are which.

