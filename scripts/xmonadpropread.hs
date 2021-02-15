#!/usr/bin/env runhaskell

-- Copyright Spencer Janssen <spencerjanssen@gmail.com>
-- BSD3 (see LICENSE)
--
-- Reads from an X property on the root window and writes to standard output.
--
-- May be used together with XMonad.Hooks.DynamicLog.xmonadPropLog and a
-- status bar that doesn't support reading from properties itself, such as
-- dzen.
--
-- Usage:
--
--  $ xmonadpropread | dzen2
--
-- or
--
--  $ xmonadpropread _XMONAD_LOG_CUSTOM | dzen2

import Control.Monad
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Codec.Binary.UTF8.String as UTF8
import Foreign.C (CChar)
import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    atom <- flip fmap getArgs $ \args -> case args of
        [a] -> a
        _   -> "_XMONAD_LOG"

    d <- openDisplay ""
    xlog <- internAtom d atom False

    root  <- rootWindow d (defaultScreen d)
    selectInput d root propertyChangeMask

    let printProp = do
            mwp <- getWindowProperty8 d xlog root
            maybe (return ()) (putStrLn . decodeCChar) mwp

    printProp

    allocaXEvent $ \ep -> forever $ do
        nextEvent d ep
        e <- getEvent ep
        case e of
            PropertyEvent { ev_atom = a } | a ==  xlog -> printProp
            _ -> return ()

decodeCChar :: [CChar] -> String
decodeCChar = UTF8.decode . map fromIntegral
