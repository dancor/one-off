{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.HashMap.Strict (HashMap)
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory
import System.Environment
import System.FilePath

import Ddo
import MyDdos.MyDdos (myDdos)

type MyTime = Double

type Db = HashMap Text MyTime

getConn :: FilePath -> FilePath -> IO Connection
getConn p f = do
    let db = p </> f
    dbExisted <- doesFileExist db
    unless dbExisted $ createDirectoryIfMissing True p
    conn <- handleSqlError $ connectSqlite3 db
    unless dbExisted . withTransaction conn $ \c -> run c (
        "CREATE TABLE dd (desc TEXT NOT NULL, time REAL, PRIMARY KEY (desc))")
        [] >> return ()
    return conn

tryD :: Connection -> Ddo -> IO (Maybe Text)
tryD conn (Ddo days d) = do
    ret <- withTransaction conn $ \c -> quickQuery c (
        "SELECT time FROM dd WHERE desc = ?") [toSql d]
    case ret of
      [[SqlDouble t]] -> do
        thenDay <- localDay . zonedTimeToLocalTime <$>
            utcToLocalZonedTime (posixSecondsToUTCTime $ realToFrac t)
        curDay <- localDay . zonedTimeToLocalTime <$> getZonedTime
        pure $ if curDay `diffDays` thenDay >= fromIntegral days
          then Just d else Nothing
      _ -> pure $ Just d

markD :: Connection -> Text -> IO ()
markD conn d = case filter ((== d) . dDesc) myDdos of
  [] -> error $ "Not in list of daily-dos: " ++ show d
  _ -> do
    t <- realToFrac <$> getPOSIXTime
    withTransaction conn $ \c -> run c 
        "REPLACE INTO dd (desc, time) VALUES (?, ?)"
        [toSql d, toSql (t :: Double)] >> return ()

mainArgs :: [String] -> IO ()
mainArgs args = do
    home <- getHomeDirectory
    let p = home </> ".local" </> "etc" </> "daily-dos"
        f = "dd.db"
    conn <- getConn p f
    mapM_ (markD conn . T.pack) args
    mapM (tryD conn) myDdos >>= T.putStrLn . T.intercalate " " . sort . catMaybes
    disconnect conn

main :: IO ()
main = getArgs >>= mainArgs
