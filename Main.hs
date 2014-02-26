{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where 

import Control.Exception(try, SomeException, handle)
import Control.Applicative ((<|>))
import Database.HDBC (SqlValue, quickQuery', fromSql, disconnect, handleSql, SqlError)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Text.CSV (Record, printCSV)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format(parseTime, formatTime)
import System.Exit (exitSuccess, exitFailure)
import System.Locale(defaultTimeLocale)
import Data.Maybe(fromMaybe)
import System.FilePath (FilePath, isValid)
import System.Directory (getPermissions, Permissions(readable), doesFileExist)
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

class ConvertibleDate a b where
    convertDate :: UTCTime -> a -> b
instance ConvertibleDate YYMMDDDate MMDDYYDate where
    convertDate defaultTime rawDate = 
        MMDDYYDate $ formatTime defaultTimeLocale "%-m/%-d/%Y" rawTime
            where rawTime = convertDate defaultTime rawDate :: UTCTime
instance ConvertibleDate YYMMDDDate UTCTime where
    convertDate defaultTime rawDate = 
        fromMaybe defaultTime $ parseTime defaultTimeLocale "%Y-%m-%d" (fromYYMMDDDate rawDate)

-- FIXME use safeFromSql to catch parse errors
parseRow:: UTCTime -> [SqlValue] -> Record
parseRow defaultTime (_:_:name:_:location:rawDate:comments:lat:long:[]) = [fromSql name,"","","x", fromSql comments, fromSql location, fromSql lat, fromSql long, date,"","","","casual","1","","N","","",""]
    where date = fromMMDDYYDate $ convertDate defaultTime $ YYMMDDDate $ fromSql rawDate
parseRow _ _ = [""]

data Options = Options {
        defaultDate :: IO UTCTime,
        inputFileName :: IO FilePath,
        outputFileName :: IO FilePath,
        cutOffDate :: Maybe UTCTime
    }

defaultOptions :: Options
defaultOptions = Options {
        defaultDate = getToday,
        inputFileName = return "AUSBDBList.sql",
        outputFileName = return "ebird.csv",
        cutOffDate = Nothing
    }

getToday :: IO UTCTime
getToday = getCurrentTime

options :: [OptDescr (Options -> IO Options)]
options = [
        Option ['D'] ["default-date"] (ReqArg parseDefaultDate "DATE") "default date to use YYYY-MM-DD",
        Option ['d'] ["cutoff-date"] (ReqArg parseCutOffDate "DATE") "only export events after this date YYYY-MM-DD",
        Option ['v'] ["version"] (NoArg showVersion) "show version number",
        Option ['h','?'] ["help"] (NoArg showHelp) "show version number",
        Option ['i'] ["input"] (ReqArg parseInputFileName "FILE") "Path to input SQL backup from iOS Australian Birds",
        Option ['o'] ["output"] (ReqArg parseOutputFileName "FILE") "Path to export CSV file for submission to eBirds.org"
    ]

parseDefaultDate :: String -> Options -> IO Options
parseDefaultDate suppliedDate opts = do
    date <- defaultDate opts
    return $ opts { defaultDate = return $ convertDate date $ YYMMDDDate suppliedDate }

parseCutOffDate :: String -> Options -> IO Options
parseCutOffDate suppliedDate opts = do
    return $ opts { cutOffDate = parseTime defaultTimeLocale "%Y-%m-%d" suppliedDate }

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

-- FIXME This actually runs all checks, so e.g. if the file doesn't exist, we still try
-- and check permissions on it...
findFilePathProblem :: FilePath -> IO (Maybe String)
findFilePathProblem suppliedFileName = do
    let fileNameValid = if isValid suppliedFileName then 
                            Nothing else 
                            Just $ "Input filename \"" ++ suppliedFileName ++ "\" is invalid"
    exists <- doesFileExist suppliedFileName 
    let fileExists = if exists then 
                        Nothing else
                        Just $ "Input filename \"" ++ suppliedFileName ++ "\" doesn't exist"
    permE <- (try $ getPermissions suppliedFileName) :: IO (Either SomeException Permissions)
    let fileReadable = case permE of 
            Left _ -> Just $ "Input filename \"" ++ suppliedFileName ++ "\" cannot have it's permissions read"
            Right permissions -> 
                if readable permissions then 
                    Nothing else
                    Just $ "Input filename \"" ++ suppliedFileName ++ "\" is not readable"
    return $ fileNameValid <|> fileExists <|> fileReadable 

parseInputFileName :: FilePath -> Options -> IO Options
parseInputFileName suppliedFileName opts = do
    filePathError <- findFilePathProblem suppliedFileName
    case filePathError of
        Just errorString -> do
            putStrLn errorString
            exitFailure
        Nothing -> return $ opts { inputFileName = return suppliedFileName }

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
    conn <- handle ((\_ -> do 
        putStrLn $ "Failed opening database in \"" ++ inFileName ++ "\""
        exitFailure):: SomeException -> IO a) $ connectSqlite3 inFileName
    rows <- handleSql (\_ -> do
        putStrLn "Failed to query table \"tblListBirds\""
        exitFailure) $ quickQuery' conn "SELECT * FROM tblListBirds" []
    writeFile outFileName $ printCSV $ map (parseRow date) rows
    disconnect conn
    exitSuccess


