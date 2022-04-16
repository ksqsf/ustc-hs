module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Time.LocalTime
import           Network.HTTP.Client as HTTP
import qualified Network.Wreq.Session as S
import           System.Environment
import           System.Exit
import           System.IO.Unsafe
import           Text.Printf
import qualified Data.Text as Text
import           USTC.EPC
  ( Category(..)
  , Weekday(..)
  , TimeSlot(..)
  )
import qualified USTC.EPC as EPC
import qualified USTC.Mail as Email

{-# NOINLINE botTag #-}
botTag :: String
botTag = fromMaybe "初号机" $ unsafePerformIO (lookupEnv "TAG")

{-# NOINLINE epcUser  #-}
epcUser :: String
epcUser = unsafePerformIO $ getEnv "EPC_USER"

{-# NOINLINE epcPass  #-}
epcPass :: String
epcPass = unsafePerformIO $ getEnv "EPC_PASS"

{-# NOINLINE emailUser  #-}
emailUser :: String
emailUser = unsafePerformIO $ getEnv "EMAIL_USER"

{-# NOINLINE emailPass  #-}
emailPass :: String
emailPass = unsafePerformIO $ getEnv "EMAIL_PASS"

-- |EPC 选课配置
epcConfig :: EPC.Config
epcConfig = EPC.Config epcUser epcPass
        [Drama, SituationalDialogue, TopicalDiscussion]
        [ (Weekday 1, [Morning2, Afternoon1, Afternoon2, Night])
        , (Weekday 2, [Morning2, Afternoon1, Afternoon2, Night])
        , (Weekday 3, [Morning2, Afternoon2])
        , (Weekday 4, [Morning2, Afternoon1, Night])
        , (Weekday 5, [Morning2, Afternoon1, Afternoon2, Night])
        , (Weekday 6, [Morning2, Afternoon1, Afternoon2, Night])
        , (Weekday 7, [Morning2, Afternoon1, Afternoon2, Night])]
        []

-- |通知配置 (SENSITIVE)
notifyConfig :: Email.Config
notifyConfig = Email.Config "email address" "email password"

-- |程序入口
main :: IO ()
main = botLoginLoop `catches`
       [ Handler (\(ex :: HttpException) ->
                    putStrLn "登录过程出现 HTTP 异常，重新登录..." >>
                    putStrLn ("异常是：" <> show ex) >>
                    main)
       , Handler (\(_ :: EPC.ReLogin) ->
                    putStrLn "会话过期，重新登录..." >>
                    main)
       ]

-- |会话总循环
botLoginLoop :: IO ()
botLoginLoop = do
  now <- getZonedTime
  printf "[开始登陆] (%s) %s\n" botTag (show now)

  -- 会话超时设置
  let settings = HTTP.defaultManagerSettings
        { managerResponseTimeout = HTTP.responseTimeoutMicro 60_000_000 }

  -- 开始登录
  sess <- S.newSessionControl (Just (HTTP.createCookieJar [])) settings
  loginOK <- EPC.login epcConfig sess
  if loginOK
    then putStrLn "登录成功" >>
         botQueryLoop sess `catch`
           (\(ex :: SomeException) ->
               putStrLn "Query 过程出现异常，重新登录..." >>
               putStrLn ("异常是：" <> show ex) >>
               botLoginLoop)
    else putStrLn "登录失败，1秒后重试" >> delay >> botLoginLoop

-- |登录成功，开始查询是否有可选的课
botQueryLoop :: S.Session -> IO ()
botQueryLoop sess = do
  now <- getZonedTime
  printf "[开始查询] (%s) %s\n" botTag (show now)
  
  -- 查询已预定但还没上的课
  bookedNew <- retry 3 $ EPC.queryBookedNew sess >>= \case
    Nothing -> fail "查询未上课程失败"
    Just new -> return new
  if null bookedNew
    then printf "目前没有已选的课"
    else printf "目前已选: \n%s" (tabifyLns bookedNew)

  -- 查询目前已经选了哪些课
  bookedAll <- EPC.queryBookedAll sess >>= \case
    Nothing -> fail "查询已选课程失败"
    Just booked -> return booked
  let hoursSum = sum (map EPC.courseHours bookedAll)
      hoursLeft = (20.0 - hoursSum) `max` 0.0
      hoursTarget = hoursLeft `min` 4.0
      hoursTodo = sum (map EPC.courseHours bookedNew)
  if hoursSum >= 20.0
    then putStrLn "所有能选的都选上啦~~" >> notifyDone >> exitSuccess
    else printf "已选 %f 课时 (%f 课时未上)，本轮选课目标: %f 课时\n" hoursSum hoursTodo hoursTarget

  -- 查询可以选的课程
  bookable <- retry 3 $ EPC.queryBookable epcConfig sess
  let (bookableGood, bookableBad) = partition (EPC.courseChoosable epcConfig) bookable
  printf "可选课程：\n%s" (tabifyLns bookableGood)
  printf "一些课程因配置条件被过滤：\n%s" (tabifyLns bookableBad)

  -- 如果非空，则重新计算课表
  if null bookableGood
    then putStrLn "没有发现新课程"
    else calcAndSubmit sess hoursTarget bookedNew bookableGood

  botQueryLoop sess

-- |根据目前课表和新找到的课，重新规划课表，并对课表进行相应的修改
calcAndSubmit :: S.Session -> Double -> [EPC.Course] -> [EPC.Course] -> IO ()
calcAndSubmit sess hoursTarget bookedNew bookable = do
  -- 计算选课方案
  let (optimalPlan, shouldBook, shouldCancel) = plan hoursTarget bookedNew bookable
  printf "最优选课方案: \n%s" (foldMap tabifyLn optimalPlan)

  -- 根据选课方案调整课表
  let makeChanges = length shouldBook + length shouldCancel > 0
  if makeChanges
    then putStrLn "将修改课程表！！！" >>
         printf "以下课程将被取消: \n%s" (foldMap tabifyLn shouldCancel) >>
         printf "以下课程将被选中: \n%s" (foldMap tabifyLn shouldBook)
    else putStrLn "课表没有变化"
  _cancelSuccess <- mapConcurrently (EPC.cancel sess) shouldCancel
  bookSuccess    <- mapConcurrently (EPC.book sess) shouldBook

  -- 查询操作后的课表
  bookedNew' <- retry 3 $ EPC.queryBookedNew sess >>= \case
    Nothing -> return Nothing
    Just new -> return (Just new)

  -- 课表如果变化则邮件通知
  when (makeChanges || Just (sort bookedNew) /= (sort <$> bookedNew')) $
    putStrLn "课表发生变化！发送邮件通知……" >>
    notifyResults bookedNew bookable optimalPlan bookedNew' (zip shouldBook bookSuccess) shouldCancel

-- 根据 “已选但未上” “可选（可能已选）” 的课程集合，计算出最优选课计划,
-- pair 的第 2 个元素是应该 book 的，第 3 个元素是应该 cancel 的。
--
-- x 应该 book, iff x 可选，且未选，且在 optimal 里。 Book   = Optimal - Pure
-- x 应该 cancel，iff x 已选且 x 不在 optimal 里;     Cancel = Booked  - Optimal
--
-- 方案满足约束：|ans| <= 2 && forall s t in ans, s.day <> t.day
plan :: Double -> [EPC.Course] -> [EPC.Course] -> ([EPC.Course], [EPC.Course], [EPC.Course])
plan left old bookable =
  let oldS          = Set.fromList old                   -- 目前已选的课程集
      bookableS     = Set.fromList bookable              -- 可选（有可能已选）的课程集
      optimal       = sort $ dfs left [] (sort . Set.toList $ oldS `Set.union` bookableS)
      optimalS      = Set.fromList optimal
      shouldBookS   = optimalS `Set.difference` oldS
      shouldCancelS = oldS `Set.difference` optimalS
  in (optimal, Set.toList shouldBookS, Set.toList shouldCancelS)
  where dfs _ ans [] = ans  -- 能选的都选完了
        dfs 0 ans _  = ans  -- 没有更多的课时可以选
        dfs n ans (x:xs)
          | EPC.courseDate x `elem` map EPC.courseDate ans || EPC.courseHours x > n =
              -- 日期冲突, 或放不下课时了, 那么跳过
              dfs n ans xs
          | otherwise =
              dfs (n - EPC.courseHours x) (x:ans) xs

--
tabifyLn :: EPC.Course -> String
tabifyLn EPC.Course{..} = printf "%30s\t%s\t%s\t%s\t%s\n" courseTitle (show courseDate) (show courseWeekday) (show courseTime) (fromMaybe "<无信息>" courseInfo)

tabifyLns :: [EPC.Course] -> String
tabifyLns = foldMap tabifyLn

--
notifyDone :: IO ()
notifyDone = Email.send notifyConfig "[EPCBot] EPC All Clear!"  "恭喜恭喜"

notifyResults
  :: [EPC.Course]           -- 操作前的选课单，bookedNew
  -> [EPC.Course]           -- 找到的可选的新课程, bookableGood
  -> [EPC.Course]           -- 计算得出的最佳选课单, optimal
  -> Maybe [EPC.Course]     -- 操作后的选课单, bookedNew'
  -> [(EPC.Course, Bool)]   -- 新订的, shouldBook, 及其订阅结果
  -> [EPC.Course]           -- 退订的, shouldCancel
  -> IO ()
notifyResults bookedNew bookableGood optimal bookedNew' shouldBook shouldCancel =
  let tagMsg = printf "机器人编号：%s\n\n" botTag
      oldTableMsg = printf "操作前的选课单:\n%s\n\n" (tabifyLns bookedNew)
      bookableMsg = printf "找到了新的可选课程:\n%s\n\n" (tabifyLns bookableGood)
      optTableMsg = printf "根据新课程, 最优计划如下:\n%s\n\n" (tabifyLns optimal)
      newTableMsg = printf "操作后，选课单目前的实际情况:\n%s\n\n" $ case bookedNew' of
        Just new' -> foldMap tabifyLn new'
        Nothing   -> "查询失败！\n"
      bookLogMsg = printf "操作日志：订阅了这些课程:\n%s\n\n" (foldMap tabifyLnWithResult shouldBook)
      cancelLogMsg = printf "操作日志：退订了这些课程:\n%s\n\n" (tabifyLns shouldCancel)
      bookCnt = length (filter id (map snd shouldBook))
      cancelCnt = length shouldCancel
      subject = printf "[EPCBot] 选中了 %d 节课, 退了 %d 节课" bookCnt cancelCnt
      body = tagMsg <> oldTableMsg <> bookableMsg <> optTableMsg <> newTableMsg <> bookLogMsg <> cancelLogMsg
  in Email.send notifyConfig (Text.pack subject) (Text.pack body)
  where bool2str True = " 选课成功 "
        bool2str False = "[选课失败]"
        tabifyLnWithResult (course, result) = bool2str result <> "\t" <> tabifyLn course

--
delay :: IO ()
delay = threadDelay 1_000_000

retry :: Int -> IO a -> IO a
retry 0 io = io
retry n io = io `catch` (\(_ :: SomeException) -> retry (n-1) io)
