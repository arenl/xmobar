----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.GReader
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  <arenielle@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Google Reader checker for Xmobar
--
-----------------------------------------------------------------------------
module Plugins.Monitors.GReader
  ( runGReader 
  , gReaderConfig
  ) 
  where

import Control.Applicative
import Data.List
import Data.Maybe
import Network.Curl
import Text.XML.HXT.Core
import Text.XML.HXT.XPath
import Plugins.Monitors.Common

authURL :: String
authURL = "https://www.google.com/accounts/ClientLogin"

url :: String
url = "http://www.google.com/reader/api/0/unread-count"

gReaderConfig :: IO MConfig
gReaderConfig = mkMConfig "<account>: <count>" ["account", "count"]

runGReader :: [String] -> Monitor String
runGReader (u:p:_) =
  io (getAuth u p >>= retrieveFeed >>= extractData) >>= pprint u

-- | Get authorization token
-- http://code.google.com/intl/en/apis/ahccounts/docs/AuthForInstalledApps.html#Using
getAuth :: String -> String -> IO String
getAuth u p = do
  (st, resp) <- curlGetString authURL [ CurlPostFields [ "Email="  ++ u
                                                       , "Passwd=" ++ p
                                                       , "service=reader" ]
                                      , CurlCookieJar "cookies" ]
  case mapMaybe (stripPrefix "Auth=") $ lines resp of
    []    -> fail $ show st
    (a:_) -> return a
  
-- | Get reader feed
retrieveFeed :: String -> IO String
retrieveFeed token = do 
  [CurlHttpHeaders ["Authorization: GoogleLogin auth=" ++ token]]  
  (st,resp) <- curlGetString url params
  case st of
    CurlOK -> return resp
    _      -> fail $ show st

-- | Get number of unread messages from reader feed
extractData :: String -> IO Int
extractData str = do
  selCount <- runX $ readString [withValidate no] str
                   >>> getXPathTreesInDoc "//number[@name='count']/text()"
                   >>> getText
  return $ if null selCount
           then 0
           else read $ maximum selCount

pprint :: String -> Int -> Monitor String
pprint u n = showWithColors show n >>= parseTemplate . (u:) . pure
