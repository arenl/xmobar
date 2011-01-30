{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Parsers
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parsers needed for Xmobar, a text based status bar
--
-----------------------------------------------------------------------------

module Parsers
    ( parseString
    , parseTemplate
    , parseConfig
    , BarFragment(..)
    ) where

import Config
import Runnable
import Commands

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Perm
import Control.Monad ( liftM )

type Color = String

-- | The possible content of a "piece" of the bar
data BarFragment = Literal String -- ^ A simple string
                 | Gap Int        -- ^ Positioning (can be negative)
                 | Image String   -- ^ A XBM/XPM image
                 | SetFg (Maybe Color) [BarFragment]
                 | SetBg (Maybe Color) [BarFragment]
                 deriving (Show)
                   
-- | Runs the string parser
--   Returns [(Fragment, FgColor, BgColor)
parseString :: Config -> String -> IO [BarFragment]
parseString _ s =
  case parse (manyTill fragmentParser eof) "" s of
    Left  _ -> return $ [Literal ("Could not parse string: " ++ s)]
    Right x -> return x

fragmentParser :: Parser BarFragment
fragmentParser = choice (map try [ fgColorParser
                                 , bgColorParser
                                 , fgbgColorParser
                                 , gapParser
                                 , imageParser
                                 ] ++ [stringParser])

-- | Parses a string
stringParser :: Parser BarFragment
stringParser = liftM Literal (many1 (try escapedChar <|>
                                     noneOf (map snd charsToEscape)))
                     
-- | Returns escaped chars
escapedChar :: Parser Char
escapedChar = do
  char '&'
  escapeCode <- manyTill anyChar (char ';')
  case lookup escapeCode charsToEscape of
    Just c  -> return c
    Nothing -> fail "Unknown escape character."

-- | List of chars to escape               
charsToEscape :: [(String, Char)]
charsToEscape = [ ("lt", '<')
                , ("gt", '>')
                ]
                
-- | Foreground
fgColorParser :: Parser BarFragment
fgColorParser = do
  (color, content) <- tagParser "fg"
  case content of
    Nothing -> fail "The tag \"fg\" must not be closed in place."
    Just fs -> return (SetFg color fs)

-- | Background
bgColorParser :: Parser BarFragment
bgColorParser = do
  (color, content) <- tagParser "bg"
  case content of
    Nothing -> fail "The tag \"bg\" must not be closed in place."
    Just fs -> return (SetBg color fs)

-- | The old foreground and background tag
fgbgColorParser :: Parser BarFragment
fgbgColorParser = do
  (color, content) <- tagParser "fc"
  case content of
    Nothing -> fail "The tag \"fc\" must not be closed in place."
    Just fs -> case color of
      Just c  -> case break (== ',') c of 
        (f, ',' : b) -> return (SetFg (Just f) [SetBg (Just b) fs])
        (f, _)       -> return (SetFg (Just f) [SetBg Nothing fs])
      Nothing -> return (SetFg Nothing [SetBg Nothing fs])

-- | Spacing
gapParser :: Parser BarFragment
gapParser = do
  (mv, c) <- tagParser "p"
  case c of
    Just _ -> fail "The tag \"p\" must be closed in place."
    Nothing  -> case mv of
      Nothing -> fail "The tag \"p\" requires a value."
      Just v  -> return (Gap $ read v)

imageParser :: Parser BarFragment
imageParser = do
  (mv, c) <- tagParser "i"
  case c of
    Just _  -> fail "The tag \"i\" must be closed in place."
    Nothing -> case mv of
      Nothing -> fail "The tag \"i\" requires a value."
      Just v  -> return (Image v)
  
-- | Tag parser
--   Accepts the name of the tag, returns (Value, Maybe Content)
tagParser :: String -> Parser (Maybe String, Maybe [BarFragment])
tagParser t = do
  char '<' >> string t
  value <- liftM Just (char '=' >> tagValue) <|>
           (lookAhead (char '>') >> return Nothing)
  content <- liftM Just (char '>' >> (manyTill fragmentParser
                                      (try $ string $ "</" ++ t ++ ">"))) <|>
             (string "/>" >> return Nothing)
  return (value, content)

tagValue :: Parser String
tagValue = manyTill anyChar (lookAhead $ (try (string "/>") >> return '*') <|> char '>')

-- | Parses the output template string
templateStringParser :: Config -> Parser (String,String,String)
templateStringParser c = do
  s   <- allTillSep c
  com <- templateCommandParser c
  ss  <- allTillSep c
  return (com, s, ss)

-- | Parses the command part of the template string
templateCommandParser :: Config -> Parser String
templateCommandParser c =
  let chr = char . head . sepChar
  in  between (chr c) (chr c) (allTillSep c)

-- | Combines the template parsers
templateParser :: Config -> Parser [(String,String,String)]
templateParser = many . templateStringParser

-- | Actually runs the template parsers
parseTemplate :: Config -> String -> IO [(Runnable,String,String)]
parseTemplate c s =
    do str <- case parse (templateParser c) "" s of
                Left _  -> return [("","","")]
                Right x -> return x
       let cl = map alias (commands c)
           m  = Map.fromList $ zip cl (commands c)
       return $ combine c m str

-- | Given a finite "Map" and a parsed template produce the resulting
-- output string.
combine :: Config -> Map.Map String Runnable -> [(String, String, String)] -> [(Runnable,String,String)]
combine _ _ [] = []
combine c m ((ts,s,ss):xs) = (com, s, ss) : combine c m xs
    where com  = Map.findWithDefault dflt ts m
          dflt = Run $ Com ts [] [] 10

allTillSep :: Config -> Parser String
allTillSep = many . noneOf . sepChar

stripComments :: String -> String
stripComments = unlines . map (drop 5 . strip False . (replicate 5 ' '++)) . lines
    where strip m ('-':'-':xs) = if m then "--" ++ strip m xs else ""
          strip m ('\\':xss) = case xss of
                                '\\':xs -> '\\' : strip m xs
                                _ -> strip m $ drop 1 xss
          strip m ('"':xs) = '"': strip (not m) xs
          strip m (x:xs) = x : strip m xs
          strip _ [] = []

-- | Parse the config, logging a list of fields that were missing and replaced
-- by the default definition.
parseConfig :: String -> Either ParseError (Config,[String])
parseConfig = runParser parseConf fields "Config" . stripComments
    where
      parseConf = do
        many space
        sepEndSpc ["Config","{"]
        x <- perms
        eof
        s <- getState
        return (x,s)

      perms = permute $ Config
              <$?> pFont         <|?> pBgColor
              <|?> pFgColor      <|?> pPosition
              <|?> pBorder       <|?> pBdColor
              <|?> pLowerOnStart <|?> pCommands
              <|?> pSepChar      <|?> pAlignSep
              <|?> pTemplate

      fields    = [ "font", "bgColor", "fgColor", "sepChar", "alignSep"
                  , "border", "borderColor" ,"template", "position"
                  , "lowerOnStart", "commands"]
      pFont     = strField font     "font"
      pBgColor  = strField bgColor  "bgColor"
      pFgColor  = strField fgColor  "fgColor"
      pBdColor  = strField borderColor "borderColor"
      pSepChar  = strField sepChar  "sepChar"
      pAlignSep = strField alignSep "alignSep"
      pTemplate = strField template "template"

      pPosition     = field position     "position"     $ tillFieldEnd >>= read' "position"
      pLowerOnStart = field lowerOnStart "lowerOnStart" $ tillFieldEnd >>= read' "lowerOnStart"
      pBorder       = field border       "border"       $ tillFieldEnd >>= read' "border"
      pCommands     = field commands     "commands"     $ readCommands

      staticPos = do string "Static"
                     wrapSkip (string "{")
                     p <- many (noneOf "}")
                     wrapSkip (string "}")
                     string ","
                     return ("Static {"  ++ p  ++ "}")
      tillFieldEnd = staticPos <|> many (noneOf ",}\n\r")

      commandsEnd  = wrapSkip (string "]") >> oneOf "},"
      readCommands = manyTill anyChar (try commandsEnd) >>= read' commandsErr . flip (++) "]"

      strField e n = field e n . between (strDel "start" n) (strDel "end" n) . many $ noneOf "\"\n\r"
      strDel   t n = char '"' <?> strErr t n
      strErr   t n = "the " ++ t ++ " of the string field " ++ n ++ " - a double quote (\")."

      wrapSkip   x = many space >> x >>= \r -> many space >> return r
      sepEndSpc    = mapM_ (wrapSkip . try . string)
      fieldEnd     = many $ space <|> oneOf ",}"
      field  e n c = (,) (e defaultConfig) $
                     updateState (filter (/= n)) >> sepEndSpc [n,"="] >>
                     wrapSkip c >>= \r -> fieldEnd >> return r

      read' d s = case reads s of
                    [(x, _)] -> return x
                    _        -> fail $ "error reading the " ++ d ++ " field: " ++ s

commandsErr :: String
commandsErr = "commands: this usually means that a command could not be parsed.\n" ++
              "The error could be located at the begining of the command which follows the offending one."

