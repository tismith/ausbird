module Main where 

import Database.HDBC (SqlValue, quickQuery', fromSql, disconnect)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Text.CSV (Record, printCSV)
import Data.Time (UTCTime)
import Data.Time.Format(readTime, formatTime)
import System.Locale(defaultTimeLocale)

{-
 - eBird record format, CSV:
 -
 - Common Name, Genus, Species, Species Count, Species Comments, Location Name, Latitute, Logitude, Date, Start Time, State, Country, Protocol, Number of Observers, Duration, All Observations Reported, Distance Covered, Area Covered, Checklist Comments
 - See http://help.ebird.org/customer/portal/articles/973915
 -
 -
 - AUSBDBList.sql schema:
 - CREATE TABLE tblListBirds (id INTEGER PRIMARY KEY, birdID INTEGER, birdComName VARCHAR(35), birdTaxNum INTEGER, birdLocation VARCHAR(100), dateOfSighting VARCHAR(20), birdComments VARCHAR(450), gpsLat REAL, gpsLong REAL);
-}

-- FIXME: use parseTime to catch parse errors
fixDate :: String -> String
fixDate rawDate = formatTime defaultTimeLocale "%-m/%-d/%Y" rawTime
    where rawTime = readTime defaultTimeLocale "%Y-%m-%d" rawDate :: UTCTime

parseRow:: [SqlValue] -> Record
parseRow (_:_:name:_:location:rawDate:comments:lat:long:[])= [fromSql name,"","","x", fromSql comments, fromSql location, fromSql lat, fromSql long, date,"","","","casual","1","","N","","",""]
    where date = fixDate $ fromSql rawDate
parseRow _ = [""]

main = do
    conn <- connectSqlite3 "AUSBDBList.sql"
    rows <- quickQuery' conn "SELECT * FROM tblListBirds" []
    writeFile "eBird.csv" $ printCSV $ map parseRow rows
    disconnect conn


