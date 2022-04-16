{- |
Description: YJS 平台

用于自动化查询、预定学术报告。
-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
module USTC.YJS
  ( AcademicReport (..)
  , AcademicReportService (..)
  , createAcademicReportService
  )
where

import           Control.Lens ((^.))
import           Control.Monad (guard)
import qualified Data.Encoding as Encoding
import           Data.Encoding.GB18030
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (encodeUtf8)
import           Network.Wreq (FormParam(..))
import qualified Network.Wreq as HTTP
import qualified Network.Wreq.Session as Wreq
import           Text.HTML.Scalpel
import qualified USTC.CAS as CAS

up :: String
up = "https://yjs.ustc.edu.cn/bgzy/m_bgxk_up.asp"

down :: String
down = "https://yjs.ustc.edu.cn/bgzy/m_bgxk_down.asp"

-- | "学术报告" 信息
data AcademicReport = AcademicReport
  { title :: Text
  , location :: Text
  , number :: Text
  , date :: Text
  , department :: Text
  }
  deriving Show

-- | 学术报告 API
data AcademicReportService = AcademicReportService
  { query  :: IO [AcademicReport]
  -- ^ 查询当前有哪些可选的学术报告
  , select :: AcademicReport -> IO Bool
  -- ^ 预定该学术报告
  , deselect :: AcademicReport -> IO Bool
  -- ^ 取消预定该学术报告
  }

-- | 使用 yjs 平台用户信息打开学术报告接口
createAcademicReportService
  :: Text  -- ^ yjs 平台用户名
  -> Text  -- ^ yjs 平台密码
  -> IO (Maybe AcademicReportService)
createAcademicReportService username password = do
  sessionMb <- CAS.login username password "https://yjs.ustc.edu.cn/default.asp"
  case sessionMb of
    Just session -> do
      return . Just $ AcademicReportService
        { query  = queryReports_ session
        , select = selectReport_ session
        , deselect = deselectReport_ session
        }
    Nothing -> return Nothing

queryReports_ :: Wreq.Session -> IO [AcademicReport]
queryReports_ session = do
  resp <- Wreq.get session up
  let htmlBS = resp ^. HTTP.responseBody
      htmlStr = Text.pack $ Encoding.decodeLazyByteString GB18030 htmlBS

  case scrapeStringLike htmlStr scraper of
    Nothing -> return []
    Just res -> return res

  where
    scraper :: Scraper Text [AcademicReport]
    scraper = chroot ("table" @: ["name" @= "table_info"]) $ chroots ("tr" @: [hasClass "bt06"]) $ do
      fields <- texts "td"
      input <- htmls "input" >>= (return.(!!1))
      guard $ not ("disabled" `Text.isInfixOf` input)
      return $ AcademicReport
        { number = fields !! 1
        , title = fields !! 2
        , department = fields !! 5
        , location = fields !! 6
        , date = fields !! 7
        }

-- |Post to reportUrl with form data
-- selectxh: "number"
-- select: "true"
selectReport_ :: Wreq.Session -> AcademicReport -> IO Bool
selectReport_ session report = do
  let body = [ "selectxh" := report.number
             , "select" := encodeUtf8 "true" ]
  resp <- Wreq.post session up body
  return $ resp ^. HTTP.responseStatus . HTTP.statusCode == 200

deselectReport_ :: Wreq.Session -> AcademicReport -> IO Bool
deselectReport_ session report = do
  let body = [ "xuhao" := report.number <> "  "
             , "tuixuan" := encodeUtf8 "true" ]
  resp <- Wreq.post session down body
  return $ resp ^. HTTP.responseStatus . HTTP.statusCode == 200

