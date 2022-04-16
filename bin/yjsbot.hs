{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Time.LocalTime
import qualified Database.SQLite.Simple as DB
import           System.Environment
import           Text.Printf
import qualified USTC.Mail as Email
import qualified USTC.YJS as Yjs

retryTillJust :: IO (Maybe a) -> IO a
retryTillJust action = do
  action >>= \case
    Just res -> return res
    Nothing  -> do
      putStrLn "retrying..."
      threadDelay (3_000_000)
      retryTillJust action

getConfig :: IO (String, String)
getConfig = do
  username <- getEnv "USERNAME"
  password <- getEnv "PASSWORD"
  return (username, password)

getEmailConfig :: IO Email.Config
getEmailConfig = do
  username <- getEnv "EMAIL_USER"
  password <- getEnv "EMAIL_PASS"
  return $ Email.Config (Text.pack username) (Text.pack password)

main :: IO ()
main = forever $
  loop `catch`
    (\(ex :: SomeException) -> putStrLn $ "Exception ignored: " ++ show ex)

loop :: IO ()
loop = do
  now <- getZonedTime
  printf "yjsbot 循环中 %s\n" (show now)
  (user, pass) <- getConfig
  yjs <- retryTillJust $ Yjs.createAcademicReportService (Text.pack user) (Text.pack pass)
  printf "登录成功！\n"
  available <- yjs.query
  if null available
    then printf "没有查询到可选课程\n"
    else do
      printf "查询到可选课程: %s\n" (ppReports available)
      selectable <- withDB $ \conn -> do
        selected <- Set.fromList <$> dbGetSelected conn
        return $ filter (\r -> not (r.number `Set.member` selected)) available
      when (not (null selectable)) $ do
        !_ <- mapM_ yjs.select selectable
        !_ <- notify selectable
        withDB $ \conn -> mapM_ (dbInsert conn) selectable
  threadDelay (10 * 60 * 1_000_000)  -- Check once every ten minutes

ppReports :: [Yjs.AcademicReport] -> String
ppReports reports = foldMap (\r -> printf "%s %s      %s      %s\n" r.department r.title r.location r.date) reports

notify :: [Yjs.AcademicReport] -> IO ()
notify reports = do
  emailCfg <- getEmailConfig
  let subject = printf "有 %d 个新的学术报告可选" (length reports)
      content = ppReports reports
  Email.send emailCfg (Text.pack subject) (Text.pack content)

instance DB.FromRow Text where
  fromRow = DB.field

withDB :: (DB.Connection -> IO r) -> IO r
withDB callback = bracket openAndInitDB DB.close callback
  where
    openAndInitDB = do
      conn <- DB.open "yjsbot.db"
      DB.execute_ conn "CREATE TABLE IF NOT EXISTS selected_before (lecture_id TEXT PRIMARY KEY)"
      return conn

dbGetSelected :: DB.Connection -> IO [Text]
dbGetSelected conn = do
  DB.query_ conn "SELECT * FROM selected_before"

dbInsert :: DB.Connection -> Yjs.AcademicReport -> IO ()
dbInsert conn report = do
  DB.execute conn "INSERT INTO selected_before (lecture_id) VALUES (?)" (DB.Only report.number)
    `catch` (\(_ :: DB.SQLError) -> pure ())
  return ()
