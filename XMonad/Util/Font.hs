{-# LANGUAGE CPP #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Font
-- Description :  A module for abstracting a font facility over Core fonts and Xft.
-- Copyright   :  (c) 2007 Andrea Rossato and Spencer Janssen
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for abstracting a font facility over Core fonts and Xft
--
-----------------------------------------------------------------------------

module XMonad.Util.Font
    ( -- * Usage:
      -- $usage
      XMonadFont(..)
    , initXMF
    , releaseXMF
    , initCoreFont
    , releaseCoreFont
    , initUtf8Font
    , releaseUtf8Font
    , Align (..)
    , stringPosition
    , textWidthXMF
    , textExtentsXMF
    , printStringXMF
    , stringToPixel
    , pixelToString
    , fi
    ) where

import XMonad
import XMonad.Prelude
import Foreign
import Control.Exception as E
import Text.Printf (printf)
import Data.Bits ((.&.))

#ifdef XFT
import qualified Data.List.NonEmpty as NE
import Graphics.X11.Xrender
import Graphics.X11.Xft
#endif

-- Hide the Core Font/Xft switching here
data XMonadFont = Core FontStruct
                | Utf8 FontSet
#ifdef XFT
                | Xft  (NE.NonEmpty XftFont)
#endif

-- $usage
-- See "XMonad.Layout.Tabbed" or "XMonad.Prompt" for usage examples

-- | Get the Pixel value for a named color: if an invalid name is
-- given the black pixel will be returned.
stringToPixel :: (Functor m, MonadIO m) => Display -> String -> m Pixel
stringToPixel d s = fromMaybe fallBack <$> io getIt
    where getIt    = initColor d s
          fallBack = blackPixel d (defaultScreen d)

-- | Convert a @Pixel@ into a @String@.
--
-- This function removes any alpha channel from the @Pixel@, because X11
-- mishandles alpha channels and produces black.
pixelToString :: (MonadIO m) => Display -> Pixel -> m String
pixelToString d p = do
    let cm = defaultColormap d (defaultScreen d)
    (Color _ r g b _) <- io (queryColor d cm $ Color (p .&. 0x00FFFFFF) 0 0 0 0)
    return ("#" ++ hex r ++ hex g ++ hex b)
  where
    -- NOTE: The @Color@ type has 16-bit values for red, green, and
    -- blue, even though the actual type in X is only 8 bits wide.  It
    -- seems that the upper and lower 8-bit sections of the @Word16@
    -- values are the same.  So, we just discard the lower 8 bits.
    --
    -- (Strictly, X11 supports 16-bit values but no visual supported
    -- by XOrg does. It is still correct to discard the lower bits, as
    -- they are not guaranteed to be meaningful in such visuals.)
    hex = printf "%02x" . (`shiftR` 8)

econst :: a -> IOException -> a
econst = const

-- | Given a fontname returns the font structure. If the font name is
--  not valid the default font will be loaded and returned.
initCoreFont :: String -> X FontStruct
initCoreFont s = do
  d <- asks display
  io $ E.catch (getIt d) (fallBack d)
      where getIt    d = loadQueryFont d s
            fallBack d = econst $ loadQueryFont d "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

releaseCoreFont :: FontStruct -> X ()
releaseCoreFont fs = do
  d <- asks display
  io $ freeFont d fs

initUtf8Font :: String -> X FontSet
initUtf8Font s = do
  d <- asks display
  (_,_,fs) <- io $ E.catch (getIt d) (fallBack d)
  return fs
      where getIt    d = createFontSet d s
            fallBack d = econst $ createFontSet d "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"

releaseUtf8Font :: FontSet -> X ()
releaseUtf8Font fs = do
  d <- asks display
  io $ freeFontSet d fs

-- | When initXMF gets a font name that starts with 'xft:' it switches to the Xft backend
-- Example: 'xft: Sans-10'
initXMF :: String -> X XMonadFont
initXMF s =
#ifndef XFT
  Utf8 <$> initUtf8Font s
#else
  if xftPrefix `isPrefixOf` s then
     do dpy <- asks display
        let fonts = case wordsBy (== ',') (drop (length xftPrefix) s) of
              []       -> "xft:monospace" :| []  -- NE.singleton only in base 4.15
              (x : xs) -> x :| xs
        Xft <$> io (traverse (openFont dpy) fonts)
  else Utf8 <$> initUtf8Font s
 where
  xftPrefix = "xft:"
  openFont dpy str = xftFontOpen dpy (defaultScreenOfDisplay dpy) str
  wordsBy p str = case dropWhile p str of
    ""   -> []
    str' -> w : wordsBy p str''
     where (w, str'') = break p str'
#endif

releaseXMF :: XMonadFont -> X ()
#ifdef XFT
releaseXMF (Xft xftfonts) = do
  dpy <- asks display
  io $ mapM_ (xftFontClose dpy) xftfonts
#endif
releaseXMF (Utf8 fs) = releaseUtf8Font fs
releaseXMF (Core fs) = releaseCoreFont fs

textWidthXMF :: MonadIO m => Display -> XMonadFont -> String -> m Int
textWidthXMF _   (Utf8 fs) s = return $ fi $ wcTextEscapement fs s
textWidthXMF _   (Core fs) s = return $ fi $ textWidth fs s
#ifdef XFT
textWidthXMF dpy (Xft xftdraw) s = liftIO $ do
#if MIN_VERSION_X11_xft(0, 3, 4)
    gi <- xftTextAccumExtents dpy (toList xftdraw) s
#else
    gi <- xftTextExtents dpy (NE.head xftdraw) s
#endif
    return $ xglyphinfo_xOff gi
#endif

textExtentsXMF :: MonadIO m => XMonadFont -> String -> m (Int32,Int32)
textExtentsXMF (Utf8 fs) s = do
  let (_,rl)  = wcTextExtents fs s
      ascent  = fi $ - (rect_y rl)
      descent = fi $ rect_height rl + fi (rect_y rl)
  return (ascent, descent)
textExtentsXMF (Core fs) s = do
  let (_,a,d,_) = textExtents fs s
  return (a,d)
#ifdef XFT
#if MIN_VERSION_X11_xft(0, 3, 4)
textExtentsXMF (Xft xftfonts) _ = io $ do
  ascent  <- fi <$> xftfont_max_ascent  xftfonts
  descent <- fi <$> xftfont_max_descent xftfonts
#else
textExtentsXMF (Xft xftfonts) _ = io $ do
  ascent  <- fi <$> xftfont_ascent  (NE.head xftfonts)
  descent <- fi <$> xftfont_descent (NE.head xftfonts)
#endif
  return (ascent, descent)
#endif

-- | String position
data Align = AlignCenter | AlignRight | AlignLeft | AlignRightOffset Int
                deriving (Show, Read)

-- | Return the string x and y 'Position' in a 'Rectangle', given a
-- 'FontStruct' and the 'Align'ment
stringPosition :: (Functor m, MonadIO m) => Display -> XMonadFont -> Rectangle -> Align -> String -> m (Position,Position)
stringPosition dpy fs (Rectangle _ _ w h) al s = do
  width <- textWidthXMF dpy fs s
  (a,d) <- textExtentsXMF fs s
  let y = fi $ ((h - fi (a + d)) `div` 2) + fi a;
      x = case al of
            AlignCenter -> fi (w `div` 2) - fi (width `div` 2)
            AlignLeft   -> 1
            AlignRight  -> fi (w - (fi width + 1));
            AlignRightOffset offset -> fi (w - (fi width + 1)) - fi offset;
  return (x,y)

printStringXMF :: (Functor m, MonadIO m) => Display -> Drawable -> XMonadFont -> GC -> String -> String
            -> Position -> Position -> String  -> m ()
printStringXMF d p (Core fs) gc fc bc x y s = io $ do
    setFont d gc $ fontFromFontStruct fs
    [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    setForeground d gc fc'
    setBackground d gc bc'
    drawImageString d p gc x y s
printStringXMF d p (Utf8 fs) gc fc bc x y s = io $ do
    [fc',bc'] <- mapM (stringToPixel d) [fc,bc]
    setForeground d gc fc'
    setBackground d gc bc'
    io $ wcDrawImageString d p fs gc x y s
#ifdef XFT
printStringXMF dpy drw fs@(Xft fonts) gc fc bc x y s = do
  let screen   = defaultScreenOfDisplay dpy
      colormap = defaultColormapOfScreen screen
      visual   = defaultVisualOfScreen screen
  bcolor <- stringToPixel dpy bc
  (a,d)  <- textExtentsXMF fs s
#if MIN_VERSION_X11_xft(0, 3, 4)
  gi <- io $ xftTextAccumExtents dpy (toList fonts) s
#else
  gi <- io $ xftTextExtents dpy (NE.head fonts) s
#endif
  io $ setForeground dpy gc bcolor
  io $ fillRectangle dpy drw gc (x - fi (xglyphinfo_x gi))
                                (y - fi a)
                                (fi $ xglyphinfo_xOff gi)
                                (fi $ a + d)
  io $ withXftDraw dpy drw visual colormap $
         \draw -> withXftColorName dpy visual colormap fc $
#if MIN_VERSION_X11_xft(0, 3, 4)
                   \color -> xftDrawStringFallback draw color (toList fonts) (fi x) (fi y) s
#else
                   \color -> xftDrawString draw color (NE.head fonts) x y s
#endif
#endif
