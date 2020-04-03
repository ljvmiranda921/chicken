module Main where

import           Text.CSV
import           Text.Printf
import           Data.List.Split
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Dates
import           Control.Monad
import           Options.Applicative


data App = App { appPath :: String
               , appDate :: String
               }


main :: IO ()
main = execParser opts >>= runWithOptions
 where
  opts = info (parser <**> helper)
    ( fullDesc
    <> progDesc "Pass a Toggl report (in CSV) to PATH"
    <> header "chicken 🐔- a Haskell parser for my Toggl checkins"
    )
  parser = App 
    <$> argument str 
        (metavar "PATH") 
    <*> strOption
        ( long "date" 
        <> short 'd'
        <> metavar "DATE"
        <> value "today"
        <> showDefault
        <> help "Specify date (e.g. 2020-12-31, today, yesterday)")


runWithOptions :: App -> IO ()
runWithOptions (App p d) = parseCheckins p d


-- | The `parseCheckins` function takes a filepath and prints out the formatted
-- checkin from Toggl
parseCheckins :: FilePath -> String -> IO ()
parseCheckins f d = do
  header <- makeHeader "checkin" (getDate d)
  putStrLn header
  csv_file <- parseCSVFromFile f
  case csv_file of
    Right csv -> mapM_ putStrLn (parseCheckins' (tail csv))
    Left  err -> print err

-- | The `makeHeader` function returns the #dailycheckin header
makeHeader :: String -> IO String -> IO String
makeHeader c iod = liftM (f c) iod
  where f x y = x ++ " " ++ y

-- | The `getDate` function takes in a string and returns the date
getDate str = do
  Right date <- getDate' str
  let y = year date
  let m = month date
  let d = day date
  return (show y ++ "-" ++ show m ++ "-" ++ show d)

getDate' d = do
  let t = getCurrentTime >>= return . toGregorian . utctDay
  todayString <- (\(y, m, d) -> show y ++ "/" ++ show m ++ "/" ++ show d) <$> t
  Right today <- return (parseDate undefined todayString)
  return (parseDate today d)

-- | The `parseCheckins'` function reads each entry in the record, formats it,
-- and stores it in a list
parseCheckins' :: PrintfType a => [[String]] -> [a]
parseCheckins' csv = [ format record | record <- csv, record /= [""] ]

-- | The `format` function stylizes the Toggle entry so that it looks just
-- like your #dailycheckin log.
format :: PrintfType t => [String] -> t
format record = printf "- %0.2f %s #%s %s"
                       duration
                       (suffix duration)
                       project
                       entry
 where
  project  = (record !! 0)
  entry    = (record !! 2)
  duration = getDuration (record !! 3)
  suffix d | d < 1     = "hr"
           | otherwise = "hrs"

-- | The `getDuration` function first splits the time string, then applies the
-- getDuration' function to compute for the total elapsed time.
getDuration :: String -> Double
getDuration s = getDuration' time where time = map read (splitOn ":" s)

-- | The `getDuration'` function gets the hour-based representation of the work
-- duration, making it #dailycheckin-compatible.
getDuration' :: [Double] -> Double
getDuration' xs = sum [ (t / 60 ^ i) | (i, t) <- l ] where l = zip [0 ..] xs
