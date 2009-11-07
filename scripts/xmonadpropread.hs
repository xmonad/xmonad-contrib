-- Copyright Spencer Janssen <spencerjanssen@gmail.com>
-- BSD3 (see LICENSE)
-- 
-- Experimental, will add proper documentation later (famous last words)

import Control.Monad
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Codec.Binary.UTF8.String as UTF8
import Foreign.C (CChar)

main = do
    d <- openDisplay ""
    xlog <- internAtom d "_XMONAD_LOG" False

    root  <- rootWindow d (defaultScreen d)
    selectInput d root propertyChangeMask

    allocaXEvent $ \ep -> forever $ do
        nextEvent d ep
        e <- getEvent ep
        case e of
            PropertyEvent { ev_atom = a } | a ==  xlog -> do
                mwp <- getWindowProperty8 d xlog root
                maybe (return ()) (putStrLn . decodeCChar) mwp
            _ -> return ()

    return ()

decodeCChar :: [CChar] -> String
decodeCChar = UTF8.decode . map fromIntegral
