{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : xmonad-status.hs
-- Copyright   : (c) Don Stewart 2007
-- License     : BSD3-style
-- Maintainer  : dons@cse.unsw.edu.au
--
-- An external statusbar-client for xmonad.
--
-- Prints the workspaces in a simple form, read from the logging output
-- of xmonad.
--
-- An example use:
--
--
{-

#!/bin/sh
#
# launch xmonad, with a couple of dzens to run the status bar
# send xmonad state over a named pipe
#

FG='#a8a3f7' 
BG='#3f3c6d' 
FONT="-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-1"

PATH=/home/dons/bin:$PATH

# simple xmonad use, no interactive status bar.
#
#clock | dzen2 -ta r -fg $FG -bg $BG -fn $FONT &
#exec xmonad

#
# with a pipe talking to an external program
#
PIPE=$HOME/.xmonad-status
rm -f $PIPE
/sbin/mkfifo -m 600 $PIPE
[ -p $PIPE ] || exit

# launch the external 60 second clock, and launch the workspace status bar
clock                 | dzen2 -e '' -x 300 -w 768 -ta r -fg $FG -bg $BG -fn $FONT &
xmonad-status < $PIPE | dzen2 -e '' -w 300 -ta l -fg $FG -bg $BG -fn $FONT &

# now go for it
xmonad > $PIPE &

# wait for xmonad
wait $!

pkill -HUP dzen2
pkill -HUP ssh-agent
pkill -HUP -f clock
pkill -HUP -f xmonad-status

# wait for all clients
wait

-}

--
-- Creates a workspace table on the left side of the screen.
--
-- A version that perfectly emulates wmii or dwm could be distributed.
--
-----------------------------------------------------------------------------

import Data.List
import StackSet
import XMonad
import System.IO
import Text.PrettyPrint
import Control.Exception

--
-- parse the StackSet output, and print it in the form:
--
--      1 [2] 4 8
--
-- It's an example of how to write a Haskell script to hack
-- the structure defined in StackSet.hs
--

main :: IO ()
main = forever $ do s <- getLine
                    handle (\e -> throwDyn (show e ++ show s))
                           (readIO s >>= draw)
  where
    forever a = catchDyn (loop a) (debug a) >> forever a
        where
            loop a    = a >> loop a
            debug a e = hPutStrLn stderr e >> forever a

--
-- All the magic is in the 'ppr' instances, below.
--
draw :: WS -> IO ()
draw s = do putStrLn . render . ppr $ s
            hFlush stdout

-- ---------------------------------------------------------------------
--
-- A simple recursive descent pretty printer for the StackSet type.
--
class Pretty a where
    ppr :: a -> Doc

-- 
-- And instances for the StackSet layers
--
instance Pretty WS where
    ppr (StackSet { current = cws   -- the different workspaces
                  , visible = vws
                  , hidden  = hws }) = ppr (sortBy tags workspaces)
      where
        -- tag each workspace with its flavour
        workspaces = C (workspace cws) : map (V . workspace) vws ++ map H hws

        -- sort them by their tags
        tags a b = (tag.unWrap) a `compare` (tag.unWrap) b

--
-- How to print each workspace kind
--
instance Pretty TaggedW where
    ppr (C w) = brackets (int (1 + fromIntegral (tag w)))                    -- [1]
    ppr (V w) = parens   (ppr w)                                -- <2>
    ppr (H w) = ppr w

-- tags are printed as integers (or map them to strings)
instance Pretty W where
-- Just print int tags:
    ppr (Workspace i s) =
        case s of
            Empty -> empty
            _     -> char ' ' <> int (1 + fromIntegral i) <> char ' '

instance Pretty a => Pretty [a] where
    ppr []     = empty
    ppr (x:xs) = ppr x <> ppr xs

-- ---------------------------------------------------------------------
-- Some type information for the pretty printer

-- We have a fixed workspace type
type W  = Workspace WorkspaceId Int
type WS = StackSet  WorkspaceId Int ScreenId

-- Introduce a newtype to distinguish different workspace flavours
data TaggedW = C !W  -- current 
             | V !W  -- visible
             | H !W  -- hidden

-- And the ability to unwrap tagged workspaces
unWrap :: TaggedW -> W
unWrap (C w) = w
unWrap (V w) = w
unWrap (H w) = w
