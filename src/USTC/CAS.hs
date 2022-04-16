{-|
Description: 统一认证服务

USTC 大部分网络服务均需要通过 CAS 认证，本模块提供统一的登录入口。
-}
module USTC.CAS
  ( login
  )
where

import           Control.Lens ((.~), (^.))
import           Data.Function ((&))
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network.Wreq (FormParam(..))
import qualified Network.Wreq as HTTP
import qualified Network.Wreq.Session as Session
import           Text.HTML.Scalpel

casLogin :: String
casLogin = "https://passport.ustc.edu.cn/login"

-- | 创建一个有 cookie 的 wreq session.
login :: Text -> Text -> Text -> IO (Maybe Session.Session)
login username password redirect = do
  session <- Session.newSession
  let opts = HTTP.defaults
           & HTTP.params .~ [("service", redirect)]

  caslt <- do
    resp <- Session.getWith opts session casLogin
    let htmlBS = resp ^. HTTP.responseBody
        scraper = attr "value" ("input" @: ["id" @= "CAS_LT"])
        caslt = scrapeStringLike htmlBS scraper
    return caslt

  ok <- do
    let body = [ "model"    := encodeUtf8 "uplogin.jsp"
               , "CAS_LT"   := caslt
               , "service"  := redirect
               , "warn"     := encodeUtf8 ""
               , "showCode" := encodeUtf8 ""
               , "username" := username
               , "password" := password
               , "button"   := encodeUtf8 "" ]
    resp <- Session.postWith opts session casLogin body
    if resp ^. HTTP.responseStatus . HTTP.statusCode == 200
      then return True
      else return False

  if ok
    then return (Just session)
    else return Nothing
