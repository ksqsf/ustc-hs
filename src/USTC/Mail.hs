{-|
Description: 电子邮件

目前仅支持发送电子邮件，用于机器人程序在后台发送通知。
-}
{-# LANGUAGE RecordWildCards #-}
module USTC.Mail
  ( Config (..)
  , send
  )
where

import qualified Data.Text as S
import qualified Data.Text.Lazy as L
import           Network.Mail.Mime
import           Network.Mail.SMTP (sendMailWithLoginTLS)

ustcMailHost :: String
ustcMailHost = "mail.ustc.edu.cn"

-- | 邮件用户信息
data Config = Config
  { login    :: S.Text
  , password :: S.Text
  }
  deriving (Show, Eq)

-- | 发送邮件
send
  :: Config  -- ^ 邮件用户信息
  -> S.Text  -- ^ 主题
  -> S.Text  -- ^ 内容
  -> IO ()
send (Config{..}) subject contents = do
  let from = Address Nothing login
      to   = Address (Just "Notify Bot") login
      mail = simpleMail' to from subject (L.fromStrict contents)
  sendMailWithLoginTLS ustcMailHost (S.unpack login) (S.unpack password) mail
