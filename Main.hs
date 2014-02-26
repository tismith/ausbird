{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where 

import Database.HDBC (SqlValue, quickQuery', fromSql, disconnect)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Text.CSV (Record, printCSV)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format(parseTime, formatTime)
import System.Exit (exitSuccess, exitFailure)
import System.Locale(defaultTimeLocale)
import Data.Maybe(fromMaybe)
import System.FilePath (FilePath, isValid)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (getOpt, ArgOrder(RequireOrder), OptDescr(Option), ArgDescr(NoArg,ReqArg), usageInfo)

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

newtype MMDDYYDate = MMDDYYDate { fromMMDDYYDate:: String }
newtype YYMMDDDate = YYMMDDDate { fromYYMMDDDate:: String }

class ConvertableDate a b where
    convertDate :: UTCTime -> a -> b
instance ConvertableDate YYMMDDDate MMDDYYDate where
    convertDate = convertYYMMDDToMMDDYY
instance ConvertableDate YYMMDDDate UTCTime where
    convertDate = convertYYMMDDToUTCTime

convertYYMMDDToMMDDYY :: UTCTime -> YYMMDDDate -> MMDDYYDate
convertYYMMDDToMMDDYY defaultTime rawDate = MMDDYYDate $ formatTime defaultTimeLocale "%-m/%-d/%Y" rawTime
    where rawTime = convertYYMMDDToUTCTime defaultTime rawDate

convertYYMMDDToUTCTime :: UTCTime -> YYMMDDDate -> UTCTime
convertYYMMDDToUTCTime defaultTime rawDate = fromMaybe defaultTime $ parseTime defaultTimeLocale "%Y-%m-%d" (fromYYMMDDDate rawDate)

-- FIXME use safeFromSql to catch parse errors
parseRow:: UTCTime -> [SqlValue] -> Record
parseRow defaultTime (_:_:name:_:location:rawDate:comments:lat:long:[]) = [fromSql name,"","","x", fromSql comments, fromSql location, fromSql lat, fromSql long, date,"","","","casual","1","","N","","",""]
    where date = fromMMDDYYDate $ convertDate defaultTime $ YYMMDDDate $ fromSql rawDate
parseRow _ _ = [""]

data Options = Options {
        defaultDate :: IO UTCTime,
        inputFileName :: IO FilePath,
        outputFileName :: IO FilePath
    }

defaultOptions :: Options
defaultOptions = Options {
        defaultDate = getToday,
        inputFileName = return "AUSDBBList.sql",
        outputFileName = return "ebird.csv"
    }

getToday :: IO UTCTime
getToday = getCurrentTime

options :: [OptDescr (Options -> IO Options)]
options = [
        Option ['D'] ["default-date"] (ReqArg parseDefaultDate "DATE") "default date to use YYYY-MM-DD",
        Option ['v'] ["version"] (NoArg showVersion) "show version number",
        Option ['h','?'] ["help"] (NoArg showHelp) "show version number",
        Option ['i'] ["input"] (ReqArg parseInputFileName "FILE") "Path to input SQL backup from iOS Australian Birds",
        Option ['o'] ["output"] (ReqArg parseOutputFileName "FILE") "Path to export CSV file for submission to eBirds.org"
    ]

parseDefaultDate :: String -> Options -> IO Options
parseDefaultDate suppliedDate opts = do
    date <- defaultDate opts
    return $ opts { defaultDate = return $ convertDate date $ YYMMDDDate suppliedDate }

showVersion :: Options -> IO Options
showVersion _ = do
    putStrLn "ausbird2ebird v0.0.1"
    exitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
    programName <- getProgName
    let header = programName ++ " - Convert iOS Aus. Birds backup SQL to eBird.org submission CSV"
    putStrLn $ usageInfo header options
    exitSuccess

parseInputFileName :: FilePath -> Options -> IO Options
parseInputFileName suppliedFileName opts =
    if isValid suppliedFileName then do
        putStrLn $ "Input filename \"" ++ suppliedFileName ++ "\" is invalid"
        exitFailure
    else 
        return $ opts { inputFileName = return suppliedFileName }

parseOutputFileName :: FilePath -> Options -> IO Options
parseOutputFileName suppliedFileName opts = 
    return $ opts { outputFileName = return suppliedFileName }

main = do
    args <- getArgs
    let (optActions, optNonOpts, optMsgs) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) optActions
    date <- defaultDate opts
    inFileName <- inputFileName opts
    outFileName <- outputFileName opts
    conn <- connectSqlite3 inFileName
    rows <- quickQuery' conn "SELECT * FROM tblListBirds" []
    writeFile outFileName $ printCSV $ map (parseRow date) rows
    disconnect conn
    exitSuccess


