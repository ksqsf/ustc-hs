{-|
Description: EPC 平台

用于自动化查询、预定 EPC 课程。
-}
{-# LANGUAGE StrictData #-}
module USTC.EPC
  ( Category (..)
  , Config (..)
  , Weekday (..)
  , TimeSlot (..)
  , Course (..)
  , ReLogin (..)

  --- * Session
  , login

  --- * Query
  , queryBookedNew
  , queryBookedAll
  , queryBookable

  --- * Effects
  , submit
  , book
  , cancel

  --- *Utils
  , courseChoosable
  )
where

import           Control.Concurrent.Async hiding (cancel)
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.ByteString.Lazy (fromStrict)
import           Data.Encoding (decodeLazyByteString)
import           Data.Encoding.GB18030 (GB18030(..))
import           Data.List
import           Data.List.Split
import           Data.Maybe (fromMaybe)
import           Data.Time.Calendar (Day(..), fromGregorian)
import           GHC.Exts
import           Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import           Network.Wreq
import           Network.Wreq.Session (Session)
import qualified Network.Wreq.Session as S
import           Network.Wreq.Types (Postable)
import           Text.HTML.Scalpel (Scraper, scrapeStringLike, texts, attr, chroots, innerHTMLs)

-- | EPC 课程类别
data Category = SituationalDialogue | TopicalDiscussion | Debate | Drama | PronunciationPractice
  deriving (Show, Eq, Ord)

epcUrlRoot, epcUrlLogin, epcUrlBooked :: String
epcUrlRoot = "http://epc.ustc.edu.cn/"
epcUrlLogin = epcUrlRoot <> "n_left.asp"
epcUrlBooked = epcUrlRoot <> "record_book.asp"

epcUrlBookable :: Category -> String
epcUrlBookable SituationalDialogue   = epcUrlRoot <> "m_practice.asp?second_id=2001"
epcUrlBookable TopicalDiscussion     = epcUrlRoot <> "m_practice.asp?second_id=2002"
epcUrlBookable Debate                = epcUrlRoot <> "m_practice.asp?second_id=2003"
epcUrlBookable Drama                 = epcUrlRoot <> "m_practice.asp?second_id=2004"
epcUrlBookable PronunciationPractice = epcUrlRoot <> "m_practice.asp?second_id=2007"

-- | EPC 平台相关配置
data Config = Config
  { epcName            :: String
  , epcPwd             :: String
  , epcCategories      :: [Category]
  -- ^ 感兴趣的课程类别
  , epcTimeSlots       :: [(Weekday, [TimeSlot])]
  -- ^ 只在这段时间内上课
  , epcExcludeTeachers :: [String]
  -- ^ 排除掉这些老师
  }
  deriving (Show, Eq, Ord)

timeSlotChoosable :: Config -> (Weekday, TimeSlot) -> Bool
timeSlotChoosable Config{..} (wd, ts) = case lookup wd epcTimeSlots of
  Just tss -> ts `elem` tss
  Nothing  -> False

excludeTeacherChoosable :: Config -> String -> Bool
excludeTeacherChoosable Config{..} teacher = teacher `notElem` epcExcludeTeachers

-- | 判断根据配置（空闲时间和已排除的老师），某个课程是否可选。
courseChoosable :: Config -> Course -> Bool
courseChoosable cfg Course{..} =
  timeSlotChoosable cfg (courseWeekday, courseTime) &&
  excludeTeacherChoosable cfg courseTeacher

-- | 周一～周日。Weekday 1 是周一，Weekday 7 是周日。
newtype Weekday = Weekday Int
  deriving (Show, Eq, Ord)

instance IsString Weekday where
  fromString "周一" = Weekday 1
  fromString "周二" = Weekday 2
  fromString "周三" = Weekday 3
  fromString "周四" = Weekday 4
  fromString "周五" = Weekday 5
  fromString "周六" = Weekday 6
  fromString "周日" = Weekday 7
  fromString _      = Weekday (-1)

-- | EPC 课程时间槽。
data TimeSlot
  = Morning1    -- ^ 8:00 ~ 9:30
  | Morning2    -- ^ 10:00 ~ 11:30
  | Afternoon1  -- ^ 14:00 ~ 15:30
  | Afternoon2  -- ^ 16:00 ~ 17:30
  | Night       -- ^ 19:00 ~ 20:30
  deriving (Eq, Ord)

instance Show TimeSlot where
  show Morning1 = "8:00~9:30"
  show Morning2 = "10:00~11:30"
  show Afternoon1 = "14:00~15:30"
  show Afternoon2 = "16:00~17:30"
  show Night = "19:00~20:30"

-- |一次课程的信息。
data Course = Course
  { courseAction   :: String       -- ^表单链接，用于取消或预定
  , courseTitle    :: String       -- ^课程标题
  , courseHours    :: Double       -- ^学时
  , courseWeekday  :: Weekday      -- ^上课是周几
  , courseDate     :: Day          -- ^上课日期
  , courseTime     :: TimeSlot     -- ^课程时间
  , courseTeacher  :: String       -- ^老师
  , courseInfo     :: Maybe String -- ^Zoom/腾讯会议信息
  , courseBooked   :: Bool         -- ^是否为已订
  }
  deriving (Show)

instance Eq Course where
  c1 == c2 =
       (courseHours c1, courseDate c1, courseTime c1, courseTitle c1, courseTeacher c1, courseAction c1)
    == (courseHours c2, courseDate c2, courseTime c2, courseTitle c2, courseTeacher c2, courseAction c2)

instance Ord Course where
  c1 <= c2 =
       (courseHours c1, courseDate c1, courseTime c1, courseTitle c1, courseTeacher c1, courseAction c1)
    <= (courseHours c2, courseDate c2, courseTime c2, courseTitle c2, courseTeacher c2, courseAction c2)

-- |绕过验证码登录, 获取Cookies
login :: Config -> Session -> IO Bool
login Config{..} sess = do
  let reqBody = [ "submit_type" := ("user_login" :: String)
                , "name"        := epcName
                , "pass"        := epcPwd
                , "user_type"   := (2 :: Int)
                , "Submit"      := ("LOG IN" :: String)]
  resp <- S.post sess epcUrlLogin reqBody
  let code = resp ^. responseStatus . statusCode
      body = decodeLazyByteString GB18030 (resp ^. responseBody)
  return $ code == 200 && not ("登录失败" `isInfixOf` body)

parseTimeSlot :: String -> TimeSlot
parseTimeSlot "08:00-09:30" = Morning1
parseTimeSlot "10:00-11:30" = Morning2
parseTimeSlot "14:00-15:30" = Afternoon1
parseTimeSlot "16:00-17:30" = Afternoon2
parseTimeSlot "19:00-20:30" = Night
parseTimeSlot _ = error "是不认识的时间格式呢"

parseCourseTime :: String -> (Day, TimeSlot)
parseCourseTime html =
  let [ dateStr, timeSlotStr ] = splitOn "<br>" html
      [ year, month, day ] = map read (splitOn "/" dateStr) :: [Integer]
      date = fromGregorian year (fromIntegral month) (fromIntegral day)
      timeSlot = parseTimeSlot timeSlotStr
  in (date, timeSlot)

-- |获取预约未上的课程列表。若请求失败或 HTML 解析失败，则返回 Nothing。
--
-- queryType 可以取 new (还未上的), all (所有已预定)。
queryBooked' :: Session -> String -> IO (Maybe [Course])
queryBooked' sess queryType = do
  (code, html) <- epcPost sess epcUrlBooked [ "querytype" := queryType ]
  return $ if code == 200
    then scrapeStringLike html $ chroots "form" bookedScraper
    else Nothing

bookedScraper :: Scraper String Course
bookedScraper = do
  action <- attr "action" "form"
  fields <- texts "td"
  fieldsHTML <- innerHTMLs "td"
  guard $ length fields == 12
  let (date, timeSlot) = parseCourseTime (fieldsHTML !! 7)
  return $ Course
    { courseAction = action
    , courseBooked = True
    , courseInfo = Just (head fields)
    , courseTitle = fields !! 1
    , courseTeacher = fields !! 2
    , courseHours = read (fields !! 3) :: Double
    , courseWeekday = fromString (fields !! 6)
    , courseTime = timeSlot
    , courseDate = date
    }

-- |获取已预约未上的课程。
queryBookedNew :: Session -> IO (Maybe [Course])
queryBookedNew sess = queryBooked' sess "new"

-- |获取历史上预约过的所有课程，包括已上的。
queryBookedAll :: Session -> IO (Maybe [Course])
queryBookedAll sess = queryBooked' sess "all"

-- |获取可预约的课程列表。若请求失败或 HTML 解析失败，则返回 Nothing。
--
-- 注：添加 &isall=all 可以取得所有课程，无论是否能选。
queryBookable' :: Category -> Session -> IO (Maybe [Course])
queryBookable' category sess = do
  (code, html) <- epcGet sess (epcUrlBookable category)
  if code == 200
    then return . scrapeStringLike html $ chroots "form" bookableScraper
    else return Nothing

bookableScraper :: Scraper String Course
bookableScraper = do
  action <- attr "action" "form"
  fields <- texts "td"
  guard $ length fields >= 12
  fieldsHTML <- innerHTMLs "td"
  let (date, timeSlot) = parseCourseTime (fieldsHTML !! 5)
  if length fields == 13
    then do guard . not $ "不能" `isInfixOf` (fields!!12)
            guard . not $ "已满" `isInfixOf` (fields!!12)
            guard . not $ "话题" `isInfixOf` (fields!!12)
            guard . not $ "相同" `isInfixOf` (fields!!12)
    else guard $ length fields == 12  -- 预约按钮不算在 fields 里面
  return $ Course
    { courseAction = action
    , courseBooked = False
    , courseInfo = Nothing
    , courseTitle = head fields
    , courseTeacher = fields !! 3
    , courseHours = read (fields !! 4) :: Double
    , courseWeekday = fromString (fields !! 2)
    , courseTime = timeSlot
    , courseDate = date
    }

-- |获取可预约的课程列表。如果请求失败，则返回空列表。
queryBookable :: Config -> Session -> IO [Course]
queryBookable Config{..} sess = do
  jobs <- mapM (\c -> async (queryBookable' c sess)) epcCategories
  fromMaybe [] <$> foldMap wait jobs

data Cmd = CmdBook | CmdCancel

instance Show Cmd where
  show CmdBook = "submit"
  show CmdCancel = "cancel"

-- |预约或取消课程. 忽视 500 错误.
submit :: Session -> Course -> Cmd -> IO Bool
submit sess Course{..} cmd = do
  let reqBody = [ "submit_type" := "book_" <> show cmd ]
      url = epcUrlRoot <> courseAction
  (code, body) <- epcPost sess url reqBody
  return $ code == 200 && not ("操作失败" `isInfixOf` body)

-- |取消一个已预定的课程 忽视 500 错误.
cancel :: Session -> Course -> IO Bool
cancel sess course =
  if courseBooked course
  then submit sess course CmdCancel
  else return True

-- |预定一个还没预定的课程 忽视 500 错误.
book :: Session -> Course -> IO Bool
book sess course =
  if courseBooked course
  then return True
  else submit sess course CmdBook

-- |过滤 StatusCodeException
statusCodeExceptionHandler :: [Int] -> HttpException -> IO (Int, String)
statusCodeExceptionHandler codes ex@(HttpExceptionRequest _ (StatusCodeException resp partialBody)) = do
  let code = resp ^. responseStatus . statusCode
      body = decodeLazyByteString GB18030 (fromStrict partialBody)
  if code `elem` codes
    then return (code, body)
    else throw ex
statusCodeExceptionHandler _ ex = throw ex

-- |
ignoredCodes :: [Int]
ignoredCodes =
  [ 500  -- Active Server Pages 错误 'ASP 0113' , 脚本超时,  /m_practice.asp , 超过了脚本运行的最长时间
  ]

postIgnoreStatus :: Postable postable => Session -> String -> postable -> [Int] -> IO (Int, String)
postIgnoreStatus sess url postable codes = handle (statusCodeExceptionHandler codes) $ do
  resp <- S.post sess url postable
  let body = decodeLazyByteString GB18030 (resp ^. responseBody)
  return (resp ^. responseStatus . statusCode, body)

getIgnoreStatus :: Session -> String -> [Int] -> IO (Int, String)
getIgnoreStatus sess url codes = handle (statusCodeExceptionHandler codes) $ do
  resp <- S.get sess url
  return (resp ^. responseStatus . statusCode, decodeLazyByteString GB18030 (resp ^. responseBody))

checkRelogin :: String -> IO ()
checkRelogin s | "登录" `isInfixOf` s = throw ReLogin
               | otherwise            = pure ()

epcGet :: Session -> String -> IO (Int, String)
epcGet sess url = do
  (code, body) <- getIgnoreStatus sess url ignoredCodes
  checkRelogin body
  return (code, body)

epcPost :: Postable postable => Session -> String -> postable -> IO (Int, String)
epcPost sess url postable = do
  (code, body) <- postIgnoreStatus sess url postable ignoredCodes
  checkRelogin body
  return (code, body)

-- | 如果返回的页面中出现了 “登录”，说明会话过期应该重新登录了。
data ReLogin = ReLogin deriving Show
instance Exception ReLogin
