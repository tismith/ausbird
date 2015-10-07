{-# LANGUAGE MultiParamTypeClasses #-}
module Main (main) where 

--Sql handling
import Database.HDBC (SqlValue, quickQuery', fromSql, disconnect)
import Database.HDBC.Sqlite3 (connectSqlite3)

--Csv handling
import Text.CSV (Record, printCSV)

--Time handling
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format(parseTime, formatTime)
import System.Locale(defaultTimeLocale)

--System and file handling
import System.Exit (exitSuccess, exitFailure)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (getOpt, ArgOrder(RequireOrder), OptDescr(Option), ArgDescr(NoArg,ReqArg), usageInfo)

--Exception handling
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Error.Util (syncIO)
import Data.EitherR (fmapLT)
import Data.Maybe(fromMaybe, mapMaybe)

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
instance ConvertibleDate MMDDYYDate UTCTime where
    convertDate defaultTime rawDate = 
        fromMaybe defaultTime $ parseTime defaultTimeLocale "%-m/%-d/%Y" (fromMMDDYYDate rawDate)

isRowOk :: UTCTime -> Maybe UTCTime -> Record -> Bool
isRowOk _ (Nothing) _ = True
isRowOk defDate (Just cutOff) (_:_:_:_:_:_:_:_:mmddyyDate:_) = convertDate defDate (MMDDYYDate mmddyyDate) > cutOff
isRowOk _ _ _ = False

parseRow:: UTCTime -> Maybe UTCTime -> [SqlValue] -> Maybe Record
parseRow defaultTime cutOff (_:_:name:_:location:rawDate:comments:lat:long:[]) = 
    let potentialRow = [fromSql name,"","","x", 
                        fromSql comments, fromSql location, 
                        fromSql lat, fromSql long, date,
                        "","","","casual","1","","N","","",""] in 
    if isRowOk defaultTime cutOff potentialRow then Just potentialRow else Nothing
        where date = fromMMDDYYDate $ convertDate defaultTime $ YYMMDDDate $ fromSql rawDate
parseRow _ _ _ = Nothing

data Options = Options {
        defaultDate :: IO UTCTime,
        inputFileName :: IO FilePath,
        outputFileName :: IO FilePath,
        cutOffDate :: Maybe UTCTime
    }

data FinalizedOptions = FinalizedOptions {
        _defaultDateF :: UTCTime,
        _inputFileNameF :: FilePath,
        _outputFileNameF :: FilePath,
        _cutOffDateF :: Maybe UTCTime
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

{-# ANN options "HLint: ignore Use string literal" #-}
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
parseCutOffDate suppliedDate opts = 
    return $ opts { cutOffDate = parseTime defaultTimeLocale "%Y-%m-%d" suppliedDate }

showVersion :: Options -> IO Options
showVersion _ = do
    programName <- getProgName
    putStrLn $ programName ++ " - v0.0.2"
    exitSuccess

showHelp :: Options -> IO Options
showHelp _ = do
    programName <- getProgName
    let header = programName ++ " - Convert iOS Aus. Birds backup SQL to eBird.org submission CSV"
    putStrLn $ usageInfo header options
    exitSuccess

parseInputFileName :: FilePath -> Options -> IO Options
parseInputFileName suppliedFileName opts = do
    fileOk <- doesFileExist suppliedFileName
    if fileOk
        then 
            return $ opts { inputFileName = return suppliedFileName }
        else do
            putStrLn $ "Input filename \"" ++ suppliedFileName ++ "\" doesn't exist"
            exitFailure

parseOutputFileName :: FilePath -> Options -> IO Options
parseOutputFileName suppliedFileName opts = 
    return $ opts { outputFileName = return suppliedFileName }

convertAusbirdFile :: Options -> ExceptT String IO FinalizedOptions
convertAusbirdFile opts = do
    date <- onError "Failed getting default date" $ defaultDate opts
    let cutOff = cutOffDate opts
    inFileName <- onError "Failed finding input filename" $ inputFileName opts
    outFileName <- onError "Failed finding output filename" $ outputFileName opts
    conn <- onError "Failed opening database" $ connectSqlite3 inFileName
    rows <- onError "Failed reading database" $ quickQuery' conn "SELECT * FROM tblListBirds" []
    onError "Failed writing csv" $ writeFile outFileName $ printCSV $ mapMaybe (parseRow date cutOff) rows
    onError "Failed closing database" $ disconnect conn
    lift $ return $ FinalizedOptions date inFileName outFileName cutOff
        where onError = \s -> fmapLT (const s) . syncIO

main :: IO ()
main = do
    args <- getArgs
    let (optActions, _, _) = getOpt RequireOrder options args
    opts <- foldl (>>=) (return defaultOptions) optActions
    e <- runExceptT $ convertAusbirdFile opts
    case e of
        Left errMsg -> putStrLn errMsg >> exitFailure
        Right (FinalizedOptions _ inF outF _) -> putStrLn ("\"" ++ inF ++ "\" converted to \"" ++ outF ++ "\" successfully") >> exitSuccess
