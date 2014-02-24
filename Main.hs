module Main (main) where 

import Database.HDBC (SqlValue, quickQuery', fromSql, disconnect)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Text.CSV (Record, printCSV)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format(parseTime, formatTime)
import System.Locale(defaultTimeLocale)
import Data.Maybe(fromMaybe)
import System.Environment (getArgs)
import System.Console.GetOpt (getOpt, ArgOrder(RequireOrder), OptDescr(Option), ArgDescr(NoArg,ReqArg))

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

fixDate :: UTCTime -> String -> String
fixDate defaultTime rawDate = formatTime defaultTimeLocale "%-m/%-d/%Y" rawTime
    where rawTime = stringToTime defaultTime rawDate

stringToTime :: UTCTime -> String -> UTCTime
stringToTime defaultTime rawDate = fromMaybe defaultTime $ parseTime defaultTimeLocale "%Y-%m-%d" rawDate

-- FIXME use safeFromSql to catch parse errors
parseRow:: UTCTime -> [SqlValue] -> Record
parseRow defaultTime (_:_:name:_:location:rawDate:comments:lat:long:[]) = [fromSql name,"","","x", fromSql comments, fromSql location, fromSql lat, fromSql long, date,"","","","casual","1","","N","","",""]
    where date = fixDate defaultTime $ fromSql rawDate
parseRow _ _ = [""]

data Options = Options {
        defaultDate :: IO UTCTime
    }

defaultOptions :: Options
defaultOptions = Options {
        defaultDate = getToday
    }

getToday :: IO UTCTime
getToday = getCurrentTime

options :: [OptDescr (Options -> IO Options)]
options = [
        Option ['D'] ["default-date"] (ReqArg parseDefaultDate "Date") "default date to use YYYY-MM-DD"
    ]

parseDefaultDate :: String -> Options -> IO Options
parseDefaultDate suppliedDate opts = do
    today <- defaultDate opts
    return $ opts { defaultDate = return $ stringToTime today suppliedDate }

main = do
    args <- getArgs
    let (optActions, optNonOpts, optMsgs) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) optActions
    date <- defaultDate opts
    conn <- connectSqlite3 "AUSBDBList.sql"
    rows <- quickQuery' conn "SELECT * FROM tblListBirds" []
    writeFile "eBird.csv" $ printCSV $ map (parseRow date) rows
    disconnect conn


