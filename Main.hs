module Main where 

import Database.HDBC
import Database.HDBC.Sqlite3

main = do
    conn <- connectSqlite3 "AUSBDBList.sql"
    r <- quickQuery' conn "SELECT * FROM tblListBirds" []
    print $ show r
    disconnect conn


