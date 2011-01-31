{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
-- New printing/parsing system by Francesco Mazzoli <f@mazzo.li>
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A status bar for the Xmonad Window Manager
--
-----------------------------------------------------------------------------

module Xmobar
    ( -- * Main Stuff
      -- $main
      X , XConf (..), runX
    , eventLoop
    -- * Program Execution
    -- $command
    , startCommand
    -- * Window Management
    -- $window
    , createWin, updateWin
    -- * Printing
    -- $print
    , drawInWin, printFragment
    ) where

import Prelude hiding (catch)
import Graphics.X11.Xlib hiding (textExtents, textWidth)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)
import Data.Bits
import Data.Maybe(fromMaybe)
import Data.Typeable (Typeable)

import Config
import Parsers hiding ( Fragment(..) )
import qualified Parsers as P
import Commands
import Runnable
import XUtil

-- $main
--
-- The Xmobar data type and basic loops and functions.

-- | The X type is a ReaderT
type X = ReaderT XConf IO

-- | The ReaderT inner component
data XConf =
    XConf { display :: Display
          , rect    :: Rectangle
          , window  :: Window
          , fontS   :: XFont
          , config  :: Config
          }

-- | Runs the ReaderT
runX :: XConf -> X () -> IO ()
runX xc f = runReaderT f xc

data WakeUp = WakeUp deriving (Show,Typeable)
instance Exception WakeUp

-- | The event loop
eventLoop :: XConf -> [[(Maybe ThreadId, TVar String)]] -> IO ()
eventLoop xc@(XConf d _ w fs c) vs = block $ do
    tv <- atomically $ newTVar []
    t  <- myThreadId
    ct <- forkIO (checker t tv [] `catch` \(SomeException _) -> return ())
    go tv ct
 where
    -- interrupt the drawing thread every time a var is updated
    checker t tvar ov = do
      nval <- atomically $ do
              nv <- mapM concatV vs
              guard (nv /= ov)
              writeTVar tvar nv
              return nv
      throwTo t WakeUp
      checker t tvar nval

    concatV = fmap concat . mapM (readTVar . snd)

    -- Continuously wait for a timer interrupt or an expose event
    go tv ct = do
      catch (unblock $ allocaXEvent $ \e ->
                 handle tv ct =<< (nextEvent' d e >> getEvent e))
            (\WakeUp -> runX xc (updateWin tv) >> return ())
      go tv ct

    -- event hanlder
    handle _ ct (ConfigureEvent {ev_window = win}) = do
      rootw <- rootWindow d (defaultScreen d)
      when (win == rootw) $ block $ do
                      killThread ct
                      destroyWindow d w
                      (r',w') <- createWin d fs c
                      eventLoop (XConf d r' w' fs c) vs

    handle tvar _ (ExposeEvent {}) = runX xc (updateWin tvar)

    handle _ _ _  = return ()

-- $command

-- | Runs a command as an independent thread and returns its thread id
-- and the TVar the command will be writing to.
startCommand :: (Runnable,String,String) -> IO (Maybe ThreadId, TVar String)
startCommand (com,s,ss)
    | alias com == "" = do var <- atomically $ newTVar is
                           atomically $ writeTVar var "Could not parse the template"
                           return (Nothing,var)
    | otherwise       = do var <- atomically $ newTVar is
                           let cb str = atomically $ writeTVar var (s ++ str ++ ss)
                           h <- forkIO $ start com cb
                           return (Just h,var)
    where is = s ++ "Updating..." ++ ss

-- $window

-- | The function to create the initial window
createWin :: Display -> XFont -> Config -> IO (Rectangle,Window)
createWin d fs c = do
  let dflt = defaultScreen d
  srs     <- getScreenInfo d
  rootw   <- rootWindow d dflt
  (as,ds) <- textExtents fs "0"
  let ht    = as + ds + 4
      (r,o) = setPosition (position c) srs (fi ht)
  win <- newWindow  d (defaultScreenOfDisplay d) rootw r o
  selectInput       d win (exposureMask .|. structureNotifyMask)
  setProperties r c d win srs
  when (lowerOnStart c) (lowerWindow d win)
  mapWindow         d win
  return (r,win)

setPosition :: XPosition -> [Rectangle] -> Dimension -> (Rectangle,Bool)
setPosition p rs ht =
  case p' of
    Top -> (Rectangle rx ry rw h, True)
    TopW a i -> (Rectangle (ax a i) ry (nw i) h, True)
    TopSize a i ch -> (Rectangle (ax a i) ry (nw i) (mh ch), True)
    Bottom -> (Rectangle rx ny rw h, True)
    BottomW a i -> (Rectangle (ax a i) ny (nw i) h, True)
    BottomSize a i ch  -> (Rectangle (ax a i) (ny' ch) (nw i) (mh ch), True)
    Static cx cy cw ch -> (Rectangle (fi cx) (fi cy) (fi cw) (fi ch), True)
    OnScreen _ p'' -> setPosition p'' [scr] ht
  where
    (scr@(Rectangle rx ry rw rh), p') =
      case p of OnScreen i x -> (fromMaybe (head rs) $ safeIndex i rs, x)
                _ -> (head rs, p)
    ny       = ry + fi (rh - ht)
    center i = rx + fi (div (remwid i) 2)
    right  i = rx + fi (remwid i)
    remwid i = rw - pw (fi i)
    ax L     = const rx
    ax R     = right
    ax C     = center
    pw i     = rw * (min 100 i) `div` 100
    nw       = fi . pw . fi
    h        = fi ht
    mh h'    = max (fi h') h
    ny' h'   = ry + fi (rh - mh h')
    safeIndex i = lookup i . zip [0..]

setProperties :: Rectangle -> Config -> Display -> Window -> [Rectangle] -> IO ()
setProperties r c d w srs = do
  a1 <- internAtom d "_NET_WM_STRUT_PARTIAL"    False
  c1 <- internAtom d "CARDINAL"                 False
  a2 <- internAtom d "_NET_WM_WINDOW_TYPE"      False
  c2 <- internAtom d "ATOM"                     False
  v  <- internAtom d "_NET_WM_WINDOW_TYPE_DOCK" False
  changeProperty32 d w a1 c1 propModeReplace $ map fi $
    getStrutValues r (position c) (getRootWindowHeight srs)
  changeProperty32 d w a2 c2 propModeReplace [fromIntegral v]

getRootWindowHeight :: [Rectangle] -> Int
getRootWindowHeight srs = foldr1 max (map getMaxScreenYCoord srs)
  where
    getMaxScreenYCoord sr = fi (rect_y sr) + fi (rect_height sr)

getStrutValues :: Rectangle -> XPosition -> Int -> [Int]
getStrutValues r@(Rectangle x y w h) p rwh =
    case p of
    OnScreen _ p'   -> getStrutValues r p' rwh
    Top             -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopW    _ _     -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    TopSize      {} -> [0, 0, st,  0, 0, 0, 0, 0, nx, nw,  0,  0]
    Bottom          -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomW _ _     -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    BottomSize   {} -> [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, nx, nw]
    Static _ _ _ _  -> getStaticStrutValues p rwh
    where st = fi y + fi h
          sb = rwh - fi y
          nx = fi x
          nw = fi (x + fi w - 1)

-- get some reaonable strut values for static placement.
getStaticStrutValues :: XPosition -> Int -> [Int]
getStaticStrutValues (Static cx cy cw ch) rwh
    -- if the yPos is in the top half of the screen, then assume a Top
    -- placement, otherwise, it's a Bottom placement
    | cy < (rwh `div` 2) = [0, 0, st,  0, 0, 0, 0, 0, xs, xe,  0,  0]
    | otherwise          = [0, 0,  0, sb, 0, 0, 0, 0,  0,  0, xs, xe]
    where st = cy + ch
          sb = rwh - cy
          xs = cx -- a simple calculation for horizontal (x) placement
          xe = xs + cw
getStaticStrutValues _ _ = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

updateWin :: TVar [String] -> X ()
updateWin v = do
  xc <- ask
  s <- io $ atomically $ readTVar v
  let (conf,rec) = (config &&& rect) xc
      l:c:r:_ = s ++ repeat ""
  ps <- io $ mapM (parseString conf) [l, c, r]
  drawInWin rec ps

-- $print

-- | Draws in and updates the window
drawInWin :: Rectangle -> [[P.Fragment]] -> X ()
drawInWin (Rectangle _ _ wid ht) ~[left',center',right'] = do
  r <- ask
  let (c,d ) = (config &&& display) r
      (w,fs) = (window &&& fontS  ) r
      fc     = fgColor c
      bc     = bgColor c
      left   = P.SetFg Nothing [P.SetBg Nothing left']
      right  = P.SetFg Nothing [P.SetBg Nothing right']
      center = P.SetFg Nothing [P.SetBg Nothing center']                               
  withColors d [bc, borderColor c] $ \[bgcolor, bdcolor] -> do
    gc <- io $ createGC  d w
    -- create a pixmap to write to and fill it with a rectangle
    p <- io $ createPixmap d w wid ht
         (defaultDepthOfScreen (defaultScreenOfDisplay d))
    -- the fgcolor of the rectangle will be the bgcolor of the window
    io $ setForeground d gc bgcolor
    io $ fillRectangle d p gc 0 0 wid ht
    offsR <- io $ liftM (fromInteger . toInteger . (wid -) . fi) $ fragmentWidth d p fs right
    offsC <- io $ liftM (fromInteger . toInteger . (\fw -> (wid - (fi fw)) `div` 2)) $ fragmentWidth d p fs center
    -- Draw the fragments
    printFragment p fs gc fc bc 1 left
    printFragment p fs gc fc bc offsR right
    printFragment p fs gc fc bc offsC center
    -- draw 1 pixel border if requested
    io $ drawBorder (border c) d p gc bdcolor wid ht
    -- copy the pixmap with the new string to the window
    io $ copyArea   d p w gc 0 0 wid ht 0 0
    -- free up everything (we do not want to leak memory!)
    io $ freeGC     d gc
    io $ freePixmap d p
    -- resync
    io $ sync       d True

drawBorder :: Border -> Display -> Drawable -> GC -> Pixel
              -> Dimension -> Dimension -> IO ()
drawBorder b d p gc c wi ht =  case b of
  NoBorder -> return ()
  TopB       -> drawBorder (TopBM 0) d p gc c w h
  BottomB    -> drawBorder (BottomBM 0) d p gc c w h
  FullB      -> drawBorder (FullBM 0) d p gc c w h
  TopBM m    -> sf >> drawLine d p gc 0 (fi m) (fi w) 0
  BottomBM m -> let rw = fi h - fi m in
                 sf >> drawLine d p gc 0 (rw + 1) (fi w) (rw + 1)
  FullBM m   -> let pad = 2 * fi m; mp = fi m in
                 sf >> drawRectangle d p gc mp mp (w - pad) (h - pad)
  where sf = setForeground d gc c
        (w, h) = (wi - 1, ht - 1)


printFragment :: Drawable -> XFont -> GC -> String -> String
                 -> Position -> P.Fragment -> X Position
printFragment dr fontst gc fc bc offs frag = do
  r <- ask
  (as, ds) <- io $ textExtents fontst ['A'..'z']
  let d                    = display r
      Rectangle _ _ _ ht   = rect r
      valign               = (fi ht + fi (as + ds)) `div` 2 - 1
  fWidth <- io $ liftM fi (fragmentWidth d dr fontst frag)
  withColors d [bc] $ \[bc'] -> do
    io $ setForeground d gc bc'
    io $ fillRectangle d dr gc offs 0 (fi fWidth) ht
  case frag of
    (P.Literal s) -> do
      io $ printString d dr fontst gc fc bc offs valign s
    (P.Gap _) -> return ()
    (P.Rectangle w h) -> withColors d [fc] $ \[fc'] -> do
      io $ setForeground d gc fc'
      io $ fillRectangle d dr gc offs
        (ti $ valign - ((as + fi h) `div` 2)) (fi w) (fi h)
    (P.Circle rad) -> withColors d [fc] $ \[fc'] -> do
      io $ setForeground d gc fc'
      io $ fillArc d dr gc offs (ti $ valign - ((as + fi rad) `div` 2))
        (fi rad) (fi rad) 2880 23040
    (P.Image f) -> withColors d [fc] $ \[fc'] -> do
      io $ setForeground d gc fc'
      (w, h, pixmap, _, _) <- io $ readBitmapFile d dr f
      io $ copyPlane d pixmap dr gc 0 0 w h offs
        (ti $ valign - ((as + fi h) `div` 2)) 1
      io $ freePixmap d pixmap
    (P.SetFg mc xs) -> case mc of
      Nothing -> liftM (fgColor . config) ask >>= (\fc' -> printFrags xs fc' bc 0)
      Just fc' -> printFrags xs fc' bc 0 
    (P.SetBg mc xs) -> case mc of
      Nothing -> liftM (bgColor . config) ask >>= (\bc' -> printFrags xs fc bc' 0)
      Just bc' -> printFrags xs fc bc' 0    
  return fWidth
  where
    ti = fromInteger . toInteger
    printFrags []              _   _   _       = return ()
    printFrags (frag' : frags) fc' bc' relOffs = do
      relOffs' <- printFragment dr fontst gc fc' bc' (offs + relOffs) frag'
      printFrags frags fc' bc' (relOffs + relOffs')
      
fragmentWidth :: Display -> Drawable -> XFont -> P.Fragment -> IO Int
fragmentWidth d _  fs (P.Literal s)     = textWidth d fs s
fragmentWidth _ _  _  (P.Gap i)         = return i
fragmentWidth d dr fs (P.SetFg _ frags) = liftM sum (mapM (fragmentWidth d dr fs) frags)
fragmentWidth d dr fs (P.SetBg _ frags) = liftM sum (mapM (fragmentWidth d dr fs) frags)
fragmentWidth _ _  _  (P.Rectangle w _) = return w
fragmentWidth _ _  _  (P.Circle rad)    = return rad
fragmentWidth d dr _  (P.Image f) =
  (io $ readBitmapFile d dr f) >>= (\(w, _, _, _, _) -> return . fi $ w)