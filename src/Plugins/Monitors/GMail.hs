-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.GMail
-- Copyright   :  (c) Oleg Smirnov
-- License     :  BSD-style (see LICENSE)
-- 
-- Maintainer  :  Oleg Smirnov <oleg.smirnov@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A GMail checker for Xmobar
--
-----------------------------------------------------------------------------

module Plugins.Monitors.GMail where

import Plugins.Monitors.Common

import Control.Monad (when)
import System.Process
import System.Exit
import System.IO

import Text.ParserCombinators.Parsec

gmailConfig :: IO MConfig
gmailConfig = mkMConfig "<account>: <count>" ["account", "count"]

data GMailInfo =
    GI { account :: String
       , count   :: Int
       } deriving (Show)

pCount :: Parser Int
pCount = do s <- manyTill digit $ (char '<')
            return $ read s

parseData :: Parser [GMailInfo]
parseData = 
    do skipRestOfLine
       skipRestOfLine
       skipTillString "<title>"
       skipTillString "Gmail - Inbox for "
       acc <- getAllBut "<"
       skipRestOfLine
       skipRestOfLine
       skipTillString "<fullcount>"
       cnt <- pCount
       manyTill skipRestOfLine eof
       return $ [GI acc cnt]

defUrl :: String
defUrl = "https://mail.google.com/a/"

defFeed :: String
defFeed = "/feed/atom"

getDomain :: String -> String
getDomain s = 
    if '@' `elem` s
       then tail $ dropWhile (/= '@') s
       else "gmail.com"

getData :: String -> String -> IO String
getData acct pass =
        do (i,o,e,p) <- runInteractiveCommand ("curl " ++" -u " ++
                                               acct ++ ":" ++ pass ++ " " ++
                                               defUrl ++ (getDomain acct) ++ defFeed)
           exit <- waitForProcess p
           let closeHandles = do hClose o
                                 hClose i
                                 hClose e
           case exit of
             ExitSuccess -> do str <- hGetContents o
                               when (str == str) $ return ()
                               closeHandles
                               return str
             _ -> do closeHandles
                     return "Could not retrieve data"

formatGMail :: [GMailInfo] -> Monitor String
formatGMail [(GI acc cnt)] =
    do cnt_ <- showWithColors show cnt
       parseTemplate [acc, cnt_]
formatGMail _ = return "N/A"

runGMail :: [String] -> Monitor String
runGMail str =
    do d <- io $ getData (head str) (head . tail $ str)
       i <- io $ runP parseData d
       formatGMail i
